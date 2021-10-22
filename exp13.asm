;日历换算：天数->日期
putcrlf macro   ;回车符
        push    dx
        push    ax
        lea     dx, crlf
        mov     ah, 09h
        int     21h
        pop     ax
        push    dx
        endm

data    segment
    yrtxt   db      'Input your year: ','$'     ;输入年提示字符串
    mthtxt  db      'Input your month: ','$'    ;输入月提示字符串
    maxlen  db      8h                          ;最多允许接收的字符个数（包含回车符0DH）（缓冲区首地址）
    actlen  db      ?                           ;留空，用于自动回填实际输入字符个数（不含0DH）
    nmbuf   db      8h dup(0)                   ;预设缓冲区（真实的字符串起始地址）
    crlf    db      0dh,0ah,'$'                 ;预设回车符 换行符 结束符
    year    db      8h dup(0);TODO 多长足够，是否可以这么定义
    month   db      6h dup(0);TODO 多长足够，是否可以这么定义
    yrmin   db      '0001',0dh,0ah,'$'          ;最小年份
    yrmax   db      '2999',0dh,0ah,'$'          ;最大年份
    mthmin  db      '01',0dh,0ah,'$'            ;最小月
    mthmax  db      '12',0dh,0ah,'$'            ;最大月
    mth     db      31                          ;平年
            db      28                          ;如果闰年就加一
            db      31
            db      30
            db      31
            db      30
            db      31
            db      31
            db      30
            db      31
            db      30
            db      31
    cmprslt db      0                           ;字符串比较结果
data    ends

code    segment
    assume  cs:code,ds:data
    main    proc    far
    start:  mov     ax, data
            mov     ds, ax                  ;装数据段
    yrtip:  mov     dx, offset yrtxt        ;提示输入年
            mov     ah, 09h
            int     21h
            lea     dx, maxlen              ;输入年
            mov     ah, 0ah
            int     21h
            putcrlf
            mov     al, actlen              ;字符串加结束符
            mov     ah, 0                   ;清空ah
            mov     si, ax
            mov     nmbuf[si],  24h
            lea     dx, nmbuf               ;输出输入的字符串
            mov     ah, 09h
            int     21h
            putcrlf
            ; call    strcmp                  ;调用字符串比较子程序
    mthtip: mov     dx, offset mthtxt       ;提示输入月
            mov     ah, 09h
            int     21h
            lea     dx, maxlen              ;输入月
            mov     ah, 0ah
            int     21h
            putcrlf
            mov     al, actlen              ;字符串加结束符
            mov     ah, 0                   ;清空ah
            mov     si, ax
            mov     nmbuf[si],  24h
            lea     dx, nmbuf               ;输出输入的字符串
            mov     ah, 09h
            int     21h
            mov     ax, 4c00h
            int     21h                     ;程序结束
    main    endp
    ;字符串比较子程序
    ;使用ax、bx
    ;params stra,strb
    ;ret    cmprslt
    ; strcmp  proc

    ; strcmp  endp
code    ends
        end     start