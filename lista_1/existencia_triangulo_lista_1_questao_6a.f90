!===========================================
!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!===========================================
!

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
