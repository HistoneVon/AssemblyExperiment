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
    ;str2num子程序
    yrnum   dw      ?                           ;年数字
    daynum  dw      ?                           ;天数字
    tbln    dw      2 dup(?)                    ;[tbl]:year/day [tbl+2]:yrnum/daynum
    ;judleap子程序
    isleap  db      0                           ;是否是闰年标志位 1是2不是
    tbll    dw      2 dup(?)                    ;[tbl]:yrnum [tbl+2]:isleap
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
            jz      leapyr
            ; jz      mthtip
            cmp     cmprslt, 3h             ;numbuf=yrmax
            jz      leapyr
            ; jz      mthtip
            mov     dx, offset yrwarn       ;输出错误提示
            mov     ah, 09h
            int     21h
            jmp     yrin
    leapyr: ; copy year to year var
            mov     ax, data
            mov     es, ax                  ;装附加段
            mov     cx, 5h                  ;五个的原因是把$符也复制过去
            lea     si, nmbuf               ;设置源串指针
            lea     di, year                ;设置目的串指针
            cld                             ;DF=0地址正向增加
            rep     movsb                   ;重复四次将年挪动至year变量处
            ; str2num
            mov     tbln,offset year
            mov     tbln+2, offset yrnum
            mov     bx, offset tbln
            call    str2num
            ; judge leap year
            mov     tbll,offset yrnum
            mov     tbll+2,offset isleap
            mov     bx, offset tbll
            call    judleap
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
    ;秦久韶算法字符串转数字子程序
    ;使用 si cx dx ax di
    ;params strg[tbl]
    ;ret    num[tbl+2]
    str2num proc    near
            push    si
            push    cx
            push    dx
            push    ax
            push    di
            mov     si, 0                   ;初始偏移量0
            sub     ax, ax                  ;ax清零
            sub     cx, cx                  ;cx用于暂存num值，初始化为0
            mov     dx, 10                  ;秦久韶算法每次乘10
            mov     di, [bx+si]             ;strg EA
    lop:    mov     al, [di]
            cmp     al, 24h                 ;如果al为24h（$）
            je      final                   ;则退出子程序
            sub     al, 30h                 ;如果al不为0则减30h，即转化为数字
            cmp     cx, 0                   ;如果cx(num)为0
            je      do_deal                 ;直接跳转到相加的位置
            push    ax                      ;保护ax
            mov     ax, cx                  ;将之前加过的cx(num)放入ax
            mul     dx                      ;src是dx中的值(10)，则ax(cx)*10
            mov     dx, 10                  ;恢复dx为10
            mov     cx, ax                  ;将乘10后的值交给cx
            pop     ax                      ;还原ax（这一位的值）
    do_deal:add     cx, ax                  ;将ax(al)中的数加入cx(num)
            mov     ax, 0                   ;ax清零
            inc     di                      ;偏移加1
            jmp     lop                     ;无条件跳转至下一位计算
    final:  mov     di, [bx+2]              ;将num的offset给di
            mov     [di],cx                 ;cx中的值传回num(yrnum/daynum)
            pop     di
            pop     ax
            pop     dx
            pop     cx
            pop     si
            ret
    str2num endp
    ;判断闰年子程序
    ;使用 si ax cx dx di
    ;params year[tbl]
    ;ret    isleap[tbl+2]
    judleap proc    near
            push    si
            push    ax
            push    cx
            push    dx
            push    di
            ;数据准备
            mov     si, [bx];year EA送si
            mov     ax, [si];以si内容为EA的内存内容（year）送ax，用作除法
            mov     cx, ax;备份年份，因为ax会被改掉
            mov     dx, 0;被除数dx_ax是32位，此处dx为0
            ;判断能否被100整除
            mov     di, 100;di用于存储除数
            div     di
            cmp     dx, 0;判断余数是否为0
            jnz     jud4;如果不能被100整除则判断能否被4整除
            ;判断能否被400整除
            mov     ax, cx;将年份从cx恢复
            mov     dx, 0;恢复dx为0（虽然此处一定为0，但保险）
            mov     di, 400
            div     di
            cmp     dx, 0
            jz      islp;如果能被400整除则跳转至是闰年islp
            jmp     isnlp;其余的不是闰年跳转至非闰年isnlp
            ;判断能否被4整除
    jud4:   mov     ax, cx;将年份从cx恢复
            mov     dx, 0;恢复dx为0
            mov     di, 4
            div     di
            cmp     dx, 0
            jnz     isnlp;如果不能被4整除则不是闰年，是闰年的不用判断走下一步
    islp:   mov     di, [bx+2]
            mov     byte ptr [di],1h
            jmp     judok
    isnlp:  mov     di, [bx+2]
            mov     byte ptr [di],2h;如果不是的直接写入2走下一步不用跳转
    judok:  pop     di
            pop     dx
            pop     cx
            pop     ax
            pop     si
            ret
    judleap endp
code    ends
        end     start