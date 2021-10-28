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
    yrtxt   db      'Input your year (0001-2999): ','$'     ;输入年提示字符串
    daytxt  db      'Input your day (001-366): ','$'    ;输入天数提示字符串
    yrwarn  db      'Your year is valid, input again: ','$'
    daywarn db      'Your day is valid, input again: ','$'
    maxlen  db      8h                          ;最多允许接收的字符个数（包含回车符0DH）（缓冲区首地址）
    actlen  db      ?                           ;留空，用于自动回填实际输入字符个数（不含0DH）
    nmbuf   db      8h dup(0)                   ;预设缓冲区（真实的字符串起始地址）
    crlf    db      0dh,0ah,'$'                 ;预设回车符 换行符 结束符
    year    db      8h dup(0);TODO 多长足够，是否可以这么定义
    day     db      6h dup(0);TODO 多长足够，是否可以这么定义
    yrmin   db      '0001',0dh,0ah,'$'          ;最小年份
    yrmax   db      '2999',0dh,0ah,'$'          ;最大年份
    daymin  db      '001',0dh,0ah,'$'           ;最小天
    daymax  db      '366',0dh,0ah,'$'           ;最大天
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
    ;strcmp子程序
    cmprslt db      0                           ;字符串比较结果（strcmp返回值）
    stra    db      8h dup(?)                   ;参数a
    strb    db      8h dup(?)                   ;参数b
    cnt     db      4                           ;比较循环次数
    tbl     dw      4 dup(?)                    ;[tbl]:cmprslt [tbl+2]:stra [tbl+4]:strb
    e       db      "e","$"
    a       db      "a","$"
    b       db      "b","$"
data    ends

code    segment
    assume  cs:code,ds:data
    main    proc    far
    start:  mov     ax, data
            mov     ds, ax                  ;装数据段
    yrtip:  mov     dx, offset yrtxt        ;提示输入年
            mov     ah, 09h
            int     21h
    yrin:   lea     dx, maxlen              ;输入年
            mov     ah, 0ah
            int     21h
            putcrlf
            mov     al, actlen              ;字符串加结束符
            mov     ah, 0                   ;清空ah
            mov     si, ax
            mov     nmbuf[si],  24h
            ; lea     dx, nmbuf               ;输出输入的字符串
            ; mov     ah, 09h
            ; int     21h
            ; putcrlf
    yrcmp1: mov     tbl,    offset cmprslt  ;送参数指针于地址表
            mov     tbl+2,  offset nmbuf
            mov     tbl+4,  offset yrmin    ;年最小值
            mov     tbl+6,  offset cnt
            mov     bx, offset tbl
            call    strcmp                  ;调用字符串比较子程序
            cmp     cmprslt, 1h             ;numbuf>yrmin
            jz      yrcmp2
            cmp     cmprslt, 3h             ;numbuf=yrmin
            jz      yrcmp2
            mov     dx, offset yrwarn       ;输出错误提示
            mov     ah, 09h
            int     21h
            jmp     yrin
    yrcmp2: mov     tbl,    offset cmprslt  ;送参数指针于地址表
            mov     tbl+2,  offset nmbuf
            mov     tbl+4,  offset yrmax    ;年最大值
            mov     tbl+6,  offset cnt
            mov     bx, offset tbl
            call    strcmp                  ;调用字符串比较子程序
            cmp     cmprslt, 2h             ;numbuf<yrmax
            ; jz      leapyr
            jz      mthtip
            cmp     cmprslt, 3h             ;numbuf=yrmax
            ; jz      leapyr
            jz      mthtip
            mov     dx, offset yrwarn       ;输出错误提示
            mov     ah, 09h
            int     21h
            jmp     yrin
    ; leapyr: 
    mthtip: mov     dx, offset daytxt       ;提示输入天
            mov     ah, 09h
            int     21h
    mthin:  lea     dx, maxlen              ;输入天
            mov     ah, 0ah
            int     21h
            putcrlf
            mov     al, actlen              ;字符串加结束符
            mov     ah, 0                   ;清空ah
            mov     si, ax
            mov     nmbuf[si],  24h
            ; lea     dx, nmbuf               ;输出输入的字符串
            ; mov     ah, 09h
            ; int     21h
            ; putcrlf
            mov     cnt,3                   ;置比较次数为3
    mthcmp1:mov     tbl,    offset cmprslt  ;送参数指针于地址表
            mov     tbl+2,  offset nmbuf
            mov     tbl+4,  offset daymin   ;天最小值
            mov     tbl+6,  offset cnt
            mov     bx, offset tbl
            call    strcmp                  ;调用字符串比较子程序
            cmp     cmprslt, 1h             ;numbuf>daymin
            jz      mthcmp2
            cmp     cmprslt, 3h             ;numbuf=daymin
            jz      mthcmp2
            mov     dx, offset daywarn      ;输出错误提示
            mov     ah, 09h
            int     21h
            jmp     mthin
    mthcmp2:mov     tbl,    offset cmprslt  ;送参数指针于地址表
            mov     tbl+2,  offset nmbuf
            mov     tbl+4,  offset daymax   ;天最大值
            mov     tbl+6,  offset cnt
            mov     bx, offset tbl
            call    strcmp                  ;调用字符串比较子程序
            cmp     cmprslt, 2h             ;numbuf<daymax
            jz      t
            cmp     cmprslt, 3h             ;numbuf=daymax
            jz      t
            mov     dx, offset daywarn      ;输出错误提示
            mov     ah, 09h
            int     21h
            jmp     mthin
    t:      lea     dx, nmbuf               ;输出输入的字符串
            mov     ah, 09h
            int     21h
    stop:   mov     ax, 4c00h
            int     21h                     ;程序结束
    main    endp
    ;字符串比较子程序
    ;使用ax bx si di cx dx
    ;params stra[tbl+2],strb[tbl+4],cnt[tbl+6]
    ;ret    cmprslt[tbl]
    strcmp  proc    near
            push    ax
            push    bx
            push    si
            push    di
            push    cx
            push    dx
            sub     cx, cx                  ;cx清零，为存放cnt做准备
            mov     si, [bx+6]              ;cnt EA
            mov     cl, [si]
            mov     si, [bx+2]              ;stra EA
            mov     di, [bx+4]              ;strb EA
            mov     bx, [bx]                ;将bx的内容为地址的内存单元送bx
            sub     ax, ax                  ;ax清零
    cmpst:  mov     ah, [si]
            mov     al, [di]
            mov     dh, ah
            mov     dl, al
            cmp     ah, al                  ;两者都没结束，进一步比较大小
            ja      cmpa
            jb      cmpb
            inc     si
            inc     di
            dec     cx
            jz      cmpe                    ;如果cx减为0就跳转相等
            jmp     cmpst                   ;循环
    cmpe:   mov     byte ptr [bx],3h        ;相等结果为3
            ; lea     dx, e
            ; mov     ah, 09h
            ; int     21h
            jmp     ok
    cmpa:   mov     byte ptr [bx],1h        ;a>b结果为1
            ; lea     dx, a
            ; mov     ah, 09h
            ; int     21h
            jmp     ok
    cmpb:   mov     byte ptr [bx],2h        ;b>a结果为2
            ; lea     dx, b
            ; mov     ah, 09h
            ; int     21h
            jmp     ok
    ok:     pop     dx
            pop     cx
            pop     di
            pop     si
            pop     bx
            pop     ax
            ret
    strcmp  endp
code    ends
        end     start