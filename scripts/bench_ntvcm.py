#!/usr/bin/env python3
"""Repeat an ntvcm command and summarize timing, cycles, and output stability."""
from __future__ import annotations

import argparse
import hashlib
import re
import statistics
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path


CYCLES_RE = re.compile(r"(?:Z80|8080)\s+cycles:\s+([0-9,]+)")


@dataclass
class RunResult:
    index: int
    seconds: float
    returncode: int
    stdout_hash: str
    stderr_hash: str
    cycles: int | None
    profile: Path | None


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def extract_cycles(stdout: bytes) -> int | None:
    text = stdout.decode("utf-8", errors="replace")
    match = CYCLES_RE.search(text)
    if not match:
        return None
    return int(match.group(1).replace(",", ""))


def render_stats(name: str, values: list[float]) -> None:
    if not values:
        return
    print(f"  {name} min    : {min(values):.6f}")
    print(f"  {name} median : {statistics.median(values):.6f}")
    print(f"  {name} mean   : {statistics.fmean(values):.6f}")
    print(f"  {name} max    : {max(values):.6f}")


def command_with_profile(command: list[str], profile_template: str | None, index: int) -> tuple[list[str], Path | None]:
    if not profile_template:
        return command, None
    profile = Path(profile_template.format(run=index))
    profile.parent.mkdir(parents=True, exist_ok=True)
    if profile.exists():
        profile.unlink()
    return [command[0], f"-g:{profile}", *command[1:]], profile


def run_once(command: list[str], cwd: Path | None, profile_template: str | None, index: int) -> RunResult:
    run_command, profile = command_with_profile(command, profile_template, index)
    start = time.perf_counter()
    proc = subprocess.run(run_command, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    elapsed = time.perf_counter() - start
    return RunResult(
        index=index,
        seconds=elapsed,
        returncode=proc.returncode,
        stdout_hash=sha256_bytes(proc.stdout),
        stderr_hash=sha256_bytes(proc.stderr),
        cycles=extract_cycles(proc.stdout),
        profile=profile,
    )


def print_run_table(results: list[RunResult]) -> None:
    print("\nRuns")
    print("  run  seconds    rc   cycles        stdout-hash")
    for result in results:
        cycles = "" if result.cycles is None else str(result.cycles)
        print(f"  {result.index:3d}  {result.seconds:8.4f}  {result.returncode:3d}  {cycles:>12}  {result.stdout_hash[:16]}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repeat", type=int, default=5, help="measured runs")
    parser.add_argument("--warmup", type=int, default=1, help="warmup runs not included in summary")
    parser.add_argument("--cwd", type=Path, default=None, help="working directory for the command")
    parser.add_argument("--profile-template", default=None,
                        help="insert -g:<path> after the executable for each run; use {run} in the path")
    parser.add_argument("--allow-output-drift", action="store_true",
                        help="do not fail if measured runs produce different stdout/stderr hashes")
    parser.add_argument("command", nargs=argparse.REMAINDER,
                        help="command to run, normally after --, for example: -- ./ntvcm -p test.com")
    args = parser.parse_args()

    command = args.command
    if command and command[0] == "--":
        command = command[1:]
    if not command:
        parser.error("missing command; use -- before the ntvcm command")
    if args.repeat <= 0 or args.warmup < 0:
        parser.error("--repeat must be positive and --warmup must be non-negative")

    total_runs = args.warmup + args.repeat
    results: list[RunResult] = []
    for index in range(1, total_runs + 1):
        result = run_once(command, args.cwd, args.profile_template, index)
        phase = "warmup" if index <= args.warmup else "run"
        profile_note = f" profile={result.profile}" if result.profile else ""
        print(f"{phase:6s} {index:3d}: {result.seconds:.4f}s rc={result.returncode}{profile_note}")
        if result.returncode != 0:
            print(f"command failed on {phase} {index}", file=sys.stderr)
            return result.returncode or 1
        if result.profile and not result.profile.exists():
            print(f"profile was not created: {result.profile}", file=sys.stderr)
            return 1
        results.append(result)

    measured = results[args.warmup:]
    print_run_table(measured)

    seconds = [result.seconds for result in measured]
    cycles = [result.cycles for result in measured if result.cycles is not None]
    print("\nSummary")
    render_stats("seconds", seconds)
    if cycles:
        render_stats("cycles ", [float(value) for value in cycles])

    stdout_hashes = {result.stdout_hash for result in measured}
    stderr_hashes = {result.stderr_hash for result in measured}
    print(f"  stdout hashes : {len(stdout_hashes)} unique")
    print(f"  stderr hashes : {len(stderr_hashes)} unique")

    if not args.allow_output_drift and (len(stdout_hashes) != 1 or len(stderr_hashes) != 1):
        print("measured runs produced different output; use --allow-output-drift to permit this", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
