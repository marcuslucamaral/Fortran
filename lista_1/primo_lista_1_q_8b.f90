!===========================================
!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!================

logical function eprimo(x)
    

!   Função que irá testar a divisibilidade do número e indicar se
!   o número é primo ou não.


implicit none
integer :: div                              !Variável que será o divisor dos testes
integer,intent(in) :: x                     !Valor de entrada fornecida pelo usuário

eprimo = .true.

if ( x == 2) then                           !Condição que indicar que 2 é primo
    eprimo = .true.
else if ( mod(x,2) == 0) then               !Condição que indica que números pares (exceção do 2) não são primos
    eprimo = .false.
else
    do div = 3, int(sqrt(real(x))), 2       !If que testa a divisibilidade do valor de entrada
        if ( MOD(x,div) == 0.) then 
            eprimo = .false.
        end if
    end do
end if

end function

!

program Chamando_primo

!   Programa para mostrar na tela todos os numéros primos
!   menores que 10.000


implicit none
integer :: i                                !Limites do loop.
logical :: eprimo                           !Função que indica se o número é primo ou não.

do i = 2,10000
    if (eprimo(i) .eqv. .true.) then
        write(*,*) i
    end if
end do

end program