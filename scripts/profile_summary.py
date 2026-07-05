#!/usr/bin/env python3
"""Summarize an ntvcm -g per-PC execution profile CSV."""
from __future__ import annotations

import argparse
import csv
import sys
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class ProfileRow:
    pc: int
    count: int
    asm: str


def parse_int(text: str) -> int:
    text = text.strip()
    if text.lower().startswith("0x"):
        return int(text, 16)
    return int(text, 10)


def load_profile(path: Path) -> list[ProfileRow]:
    rows: list[ProfileRow] = []
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
            rows.append(ProfileRow(pc=pc, count=count, asm=row.get("asm", "")))
    return rows


def format_pct(count: int, total: int) -> str:
    return f"{(count * 100.0 / total):6.2f}%" if total else "  0.00%"


def print_top_pcs(rows: list[ProfileRow], top: int, total: int) -> None:
    print(f"\nTop {top} PCs")
    print("  pc      count        pct     asm")
    for row in sorted(rows, key=lambda item: item.count, reverse=True)[:top]:
        print(f"  {row.pc:04x} {row.count:10d} {format_pct(row.count, total)}  {row.asm}")


def print_ranges(rows: list[ProfileRow], top: int, bucket_size: int, total: int) -> None:
    buckets: dict[int, int] = defaultdict(int)
    for row in rows:
        buckets[(row.pc // bucket_size) * bucket_size] += row.count

    print(f"\nTop {top} address ranges ({bucket_size}-byte buckets)")
    print("  range       count        pct")
    ranked = sorted(buckets.items(), key=lambda item: item[1], reverse=True)
    for start, count in ranked[:top]:
        end = min(0xFFFF, start + bucket_size - 1)
        print(f"  {start:04x}-{end:04x} {count:10d} {format_pct(count, total)}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("profile", type=Path, help="CSV file produced by ntvcm -g:<file>")
    parser.add_argument("--top", type=int, default=25, help="number of top rows/ranges to show")
    parser.add_argument("--bucket-size", type=parse_int, default=0x20,
                        help="address bucket size for range summaries, decimal or 0x-prefixed")
    parser.add_argument("--no-ranges", action="store_true", help="only print per-PC results")
    args = parser.parse_args()

    try:
        rows = load_profile(args.profile)
    except (OSError, ValueError) as exc:
        print(exc, file=sys.stderr)
        return 1

    total = sum(row.count for row in rows)
    print(f"Profile: {args.profile}")
    print(f"  executed PCs : {len(rows)}")
    print(f"  total hits   : {total}")

    print_top_pcs(rows, args.top, total)
    if not args.no_ranges:
        if args.bucket_size <= 0:
            print("--bucket-size must be positive", file=sys.stderr)
            return 1
        print_ranges(rows, args.top, args.bucket_size, total)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
