!================================================================================
 
 ! Purpose:
    ! Escrever uma function que receba 3 valores inteiros e determine se eles podem repre-
    !sentar os lados de um triângulo retângulo.
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
! funçao verificaçao se é triangulo valido
logical function f1_is3angle(l1, l2, l3)
implicit none
integer, intent(in)::l1,l2,l3

    if (l1 + l2 > l3 .and. l1 + l3 > l2 .and. l2 + l3 > l1) then
    f1_is3angle = .true.
    else
    f1_is3angle = .false.
    end if

end function f1_is3angle

!funçao verificaçao se e triangulo retangulo valido

logical function f2_isR3angle(l1,l2,l3)
implicit none
integer, intent(in)::l1,l2,l3
integer::h, c1,c2


    if(l1 > l2 .and. l1 > l3) then
        h  = l1
        c1 = l2
        c2 = l3
    else if (l2 > l1 .and. l2 > l3) then
        h  = l2
        c1 = l1
        c2 = l3
    else 
        h  = l3
        c1 = l1
        c2 = l2
    end if

    if (h**2 == c1**2 + c2**2) then  !!Cuidado. Aqui a comparação funciona pois são inteiros. Não faça isso com reais.
        f2_isR3angle = .true.
    else
        f2_isR3angle = .false.
    end if

end function f2_isR3angle
!
!
!
program Test_R3angle
implicit none
integer :: a, b, c
logical :: f1_is3angle, teste1, f2_isR3angle, teste2

write(*,*) 'Informe o comprimento de cada um dos três lados do triângulo (a,b e c):'
read(*,*) a, b, c

    teste1 = f1_is3angle(a,b,c)

    if (teste1 .eqv. .TRUE.) then
        Write(*,*) '--------------------------------------'
        Write(*,*) 'Esses lados podem formar um triângulo!'
        teste2 = f2_isR3angle(a,b,c)
    
        if (teste2 .eqv. .TRUE.) then
            write(*,*)'E um triângulo retângulo.'
            
        else
            write(*,*)'Mas NÃO um triângulo retângulo.'
            
        end if
    else
    
        write(*,*)'Esses lados não podem formar um triângulo.'
    
    end if


end program Test_R3angle
