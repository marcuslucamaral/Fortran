program aleatorios

    implicit none
    real:: x
    integer:: i

    do i = 1,10
        call random_number(x) ! ele gera numeros entre fechado no 0 e aberto no 1
           ! x = 2*(x-0.5) ! muda-se o intervalo entre -1 a 1
            write(*,*) i, x
    end do


end program aleatorios