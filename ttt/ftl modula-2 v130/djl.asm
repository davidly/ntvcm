        label           ; Write
        pop     hl      ; return address
        pop     bc      ; character to write
        ld      e, c    ; character to write
        ld      c, 2    ; console output
        call    0005
        jp      (hl)

