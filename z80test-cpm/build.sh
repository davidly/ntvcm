#!/bin/bash
# Builds the raxoft/z80test CP/M port: clones and patches the original
# sjasm 0.42c assembler (needed - see README.md for why sjasmplus won't
# work), then assembles each test variant as a relocating CP/M .COM that
# runs the unmodified test payload at the Spectrum's own org 0x8000.
#
# Usage: ./build.sh [variant ...]
#   variant is one of: full doc flags docflags ccf memptr (default: all)
#
# Output: <variant>.com in the current directory, e.g. doc.com

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SJASM_DIR="$SCRIPT_DIR/.sjasm-build"
SJASM_BIN="$SJASM_DIR/Sjasm/Sjasm/sjasm"

VARIANTS=("$@")
if [ ${#VARIANTS[@]} -eq 0 ]; then
    VARIANTS=(full doc flags docflags ccf memptr)
fi

if [ ! -x "$SJASM_BIN" ]; then
    echo "Building sjasm 0.42c (one-time step)..."
    rm -rf "$SJASM_DIR"
    mkdir -p "$SJASM_DIR"
    git clone --quiet https://github.com/Konamiman/Sjasm.git "$SJASM_DIR/Sjasm"
    (cd "$SJASM_DIR/Sjasm" && git checkout --quiet v0.42c)
    (cd "$SJASM_DIR/Sjasm" && git apply "$SCRIPT_DIR/patches/sjasm-0.42c-linux64-build-fix.patch")
    ln -sf Sjasm.cpp "$SJASM_DIR/Sjasm/Sjasm/sjasm.cpp"
    make -C "$SJASM_DIR/Sjasm/Sjasm" CXXFLAGS="-O2 -std=c++14"
    echo "sjasm built at $SJASM_BIN"
fi

BUILD_DIR="$(mktemp -d)"
trap 'rm -rf "$BUILD_DIR"' EXIT
cp "$SCRIPT_DIR"/src/*.asm "$BUILD_DIR"/

for v in "${VARIANTS[@]}"; do
    src="z80$v.asm"
    if [ ! -f "$BUILD_DIR/$src" ]; then
        echo "unknown variant '$v' (expected one of full doc flags docflags ccf memptr)" >&2
        exit 1
    fi
    echo "Building $v.com from $src ..."
    (
        cd "$BUILD_DIR"
        # main.asm always includes the pristine idea.asm/tests.asm chain at
        # org 0x8000; each variant file just sets the maskflags/onlyflags/
        # postccf/memptr equs and testname before including main.asm.
        cp "$src" z80test_variant.asm
        sed -i 's/include[[:space:]]*main\.asm/include main.asm/' z80test_variant.asm
        "$SJASM_BIN" z80test_variant.asm payload.bin
        # stub.asm incbins whatever file is named "z80doc_8000.bin" -
        # point it at this variant's payload via a symlink.
        ln -sf payload.bin z80doc_8000.bin
        "$SJASM_BIN" stub.asm "$v.com"
    )
    cp "$BUILD_DIR/$v.com" "$SCRIPT_DIR/$v.com"
    echo "  -> $SCRIPT_DIR/$v.com"
done

echo "Done. Run e.g.: ntvcm $SCRIPT_DIR/doc.com"
