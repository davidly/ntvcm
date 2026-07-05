#!/usr/bin/env python3
"""Compare two ntvcm -g per-PC execution profile CSV files."""
from __future__ import annotations

import argparse
import csv
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class ProfileEntry:
    count: int
    asm: str


def parse_int(text: str) -> int:
    text = text.strip()
    if text.lower().startswith("0x"):
        return int(text, 16)
    return int(text, 10)


def load_profile(path: Path) -> dict[int, ProfileEntry]:
    entries: dict[int, ProfileEntry] = {}
    with path.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        required = {"pc", "count", "asm"}
        if set(reader.fieldnames or []) < required:
            raise ValueError(f"{path}: expected CSV columns pc,count,asm")
        for line_no, row in enumerate(reader, start=2):
            try:
                pc = parse_int(row["pc"])
                count = parse_int(row["count"])
            except (TypeError, ValueError) as exc:
                raise ValueError(f"{path}:{line_no}: invalid pc/count") from exc
            if not 0 <= pc <= 0xFFFF:
                raise ValueError(f"{path}:{line_no}: pc out of range: {pc}")
            if count < 0:
                raise ValueError(f"{path}:{line_no}: negative count: {count}")
            entries[pc] = ProfileEntry(count=count, asm=row.get("asm", ""))
    return entries


def pct(delta: int, before: int) -> str:
    if before == 0:
        return "   new" if delta else " 0.00%"
    return f"{(delta * 100.0 / before):6.2f}%"


def print_changes(title: str, rows: list[tuple[int, int, int, str]], top: int) -> None:
    print(f"\n{title}")
    print("  pc      before      after      delta     pct      asm")
    for pc, before, after, asm in rows[:top]:
        delta = after - before
        print(f"  {pc:04x} {before:10d} {after:10d} {delta:10d} {pct(delta, before):>8}  {asm}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("before", type=Path, help="baseline profile CSV")
    parser.add_argument("after", type=Path, help="comparison profile CSV")
    parser.add_argument("--top", type=int, default=25, help="number of changes to show in each section")
    parser.add_argument("--csv", type=Path, default=None, help="optional per-PC diff CSV output")
    args = parser.parse_args()

    try:
        before = load_profile(args.before)
        after = load_profile(args.after)
    except (OSError, ValueError) as exc:
        print(exc, file=sys.stderr)
        return 1

    pcs = sorted(set(before) | set(after))
    changes: list[tuple[int, int, int, str]] = []
    for pc in pcs:
        before_entry = before.get(pc, ProfileEntry(0, ""))
        after_entry = after.get(pc, ProfileEntry(0, ""))
        asm = after_entry.asm or before_entry.asm
        changes.append((pc, before_entry.count, after_entry.count, asm))

    before_total = sum(entry.count for entry in before.values())
    after_total = sum(entry.count for entry in after.values())
    delta_total = after_total - before_total

    print(f"Before: {args.before}")
    print(f"After : {args.after}")
    print(f"  before total hits : {before_total}")
    print(f"  after total hits  : {after_total}")
    print(f"  delta total hits  : {delta_total} ({pct(delta_total, before_total).strip()})")
    print(f"  before PCs        : {len(before)}")
    print(f"  after PCs         : {len(after)}")

    decreases = sorted(changes, key=lambda item: item[2] - item[1])
    increases = sorted(changes, key=lambda item: item[2] - item[1], reverse=True)
    print_changes(f"Largest decreases", decreases, args.top)
    print_changes(f"Largest increases", increases, args.top)

    if args.csv:
        with args.csv.open("w", newline="", encoding="utf-8") as fh:
            writer = csv.writer(fh)
            writer.writerow(["pc", "before", "after", "delta", "asm"])
            for pc, before_count, after_count, asm in changes:
                writer.writerow([pc, before_count, after_count, after_count - before_count, asm])
        print(f"\nWrote diff CSV: {args.csv}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
