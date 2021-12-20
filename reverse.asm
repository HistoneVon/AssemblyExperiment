;内存字节逆序函数
    .model small
    .data
str     db      "1202","$"
length  db      4h
tbl     dw      2 dup(?)
    .code
main    proc    far
begin:  mov     ax, @data
        mov     ds, ax
        mov     tbl,offset str
        mov     tbl+2,offset length
        mov     bx, offset tbl
        call    reverse
        mov     ax, 4c00h
        int     21h
main    endp
;内存字节逆序函数
;使用
;params str[tbl]
;       length[tbl+2]
reverse proc    near
        
reverse endp
    end begin