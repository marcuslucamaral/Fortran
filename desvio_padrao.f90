program calcula_desvio
    implicit none
    integer:: i
    integer:: N
    real:: x, soma, media
    real:: soma2, dp,t1,t2
    
    call cpu_time(t1)
    open(101, file='data.dat',status='old',action='read')
    
    read(101,*) N
    soma = 0.
    do i =1,N
        read(101,*) x
        soma = soma + x
    
    end do
    
    media = soma/n
    write(*,*)'media',media

    rewind(101)

    read(101,*) N

    soma2=0.
    do i=1,N
        read(101,*) x
        soma2 = soma2 + (x- media)**2
    end do
    dp= sqrt(soma2/N)

    write(*,*)'Dp:',dp

    call cpu_time(t2)

    write(*,*) 'tempo:',t2-t1,'segundos.'
    end program calcula_desvio