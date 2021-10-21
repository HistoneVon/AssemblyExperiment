;日历换算：天数->日期
data    segment
intxt   db      'Input your year and month: ','$'   ;输入提示字符串
crlf    db      0dh,0ah,'$'                         ;预设回车符和换行符
data    ends
code    segment
assume  cs:code,ds:data
main    proc    far
start:  mov     ax, data
        mov     ds, ax                              ;装数据段
        mov     dx, offset intxt                    ;将输入提示字符EA送DX
        mov     ah, 09h                             ;DOS系统功能号09h（输出字符串）送AH
        int     21h                                 ;21h号ROM BIOS中断调用
        mov     ax, 4c00h
        int     21h                                 ;程序结束
main    endp
code    ends
        end     start