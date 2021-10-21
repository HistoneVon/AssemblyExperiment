;日历换算：天数->日期
data    segment
    intxt   db      'Input your year and month: ','$'   ;输入提示字符串
    maxlen  db      5h                                  ;最多允许接收的字符个数（包含回车符0DH）（缓冲区首地址）
    actlen  db      ?                                   ;留空，用于自动回填实际输入字符个数（不含0DH）
    nmbuf   db      5h dup(0)                           ;预设缓冲区（真实的字符串起始地址）
    year    db      5h dup(0);TODO 多长足够，是否可以这么定义
    month   db      3h dup(0);TODO 多长足够，是否可以这么定义
    crlf    db      0dh,0ah,'$'                         ;预设回车符 换行符 结束符
data    ends
code    segment
    assume  cs:code,ds:data
    main    proc    far
    start:  mov     ax, data
            mov     ds, ax                  ;装数据段
    intip:  mov     dx, offset intxt        ;将输入提示字符EA送DX
            mov     ah, 09h
            int     21h                     ;输出字符串
            lea     dx, maxlen              ;将缓冲区首地址maxlen的EA送DX
            mov     ah, 0ah
            int     21h                     ;键盘输入到缓冲区
            lea     dx, crlf                ;回车符EA送DX
            dec     ah
            int     21h                     ;输出字符串
            mov     ax, 4c00h
            int     21h                     ;程序结束
    main    endp
code    ends
        end     start