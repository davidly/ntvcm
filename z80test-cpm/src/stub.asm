; Tiny CP/M loader stub - loads at 0x100 (as any .COM must), then
; relocates the appended, org-0x8000-assembled z80test payload to its
; real address and jumps there, so all of its embedded absolute
; addresses (notably mem_) exactly match the official Spectrum build.

            org     0x100

start:      ld      hl,payload      ; source: payload bytes as loaded
            ld      de,0x8000       ; destination: where it was assembled to run
            ld      bc,payload_end-payload
            ldir
            jp      0x8000

payload:
            incbin  "z80doc_8000.bin"
payload_end:

; EOF ;
