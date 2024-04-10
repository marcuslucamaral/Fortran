program calcula_desvio_1
    implicit none
    integer:: i
    integer:: N
    real:: x, soma, media
    real:: soma2, dp,t1,t2
    call cpu_time(t1)
    open(101, file='data.dat',status='old',action='read')
    
    read(101,*) N
    soma = 0.
    soma2=0.
    do i =1,N
        read(101,*) x
        soma = soma + x
        soma2=soma2+x**2
    end do
    
    media = soma/n
    
    dp= sqrt(soma2/N-media**2)

    write(*,*)'media',media

    write(*,*)'Dp:',dp
    call cpu_time(t2)
    write(*,*) 'tempo:',t2-t1,'segundos.'
    end program calcula_desvio_1