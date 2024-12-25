!================================================================================
 
 ! Purpose:
    ! Escrever uma function que receba 3 valores reais e determine se eles podem representar
    !os lados de um triângulo.
    !Cada function nesta questão pode ser do tipo integer, character ou logical. Se a
    !função for do tipo integer a saída pode ser 0 ou 1, se ela for do tipo character a saída
    !pode ser uma das strings SIM ou NAO e se for do tipo logical evidentemente a saída deve
    !ser .true. ou .false.
    !
    !
    ! Record of revisions:
    ! Date_begin/end_date                 Programmer                             Description of change
    ! ========                     =============================                      =============
    ! 13/04/23                            Marcus Amaral                               Original code
    ! 13/04/23
!================================================================================
logical function exist_triang(l, m, n)


implicit none
real, intent(in):: l, m, n

    if (l + m > n .and. l + n > m .and. m + n > l) then
    exist_triang = .true.
    else
    exist_triang = .false.
    end if

End Function exist_triang

!------------------------------------------------------------------------------------------!

program test_exist_triang
implicit none
real :: a, b, c
logical :: exist_triang, teste

    write(*,*) 'Entre com o tamanho de cada lado do triangulo:'
    read(*,*) a, b, c

    teste = exist_triang(a,b,c)

    if (teste .eqv. .true.) then
    
    Write(*,*) 'Esse triangulo existe, pois pode possuir esse lados!'

    else
    
    write(*,*)'Esse triangulo nao existe, pois nao pode possuir esse lados!'

end if


end program test_exist_triang
