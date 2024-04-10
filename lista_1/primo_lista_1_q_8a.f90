!===========================================
!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!================

character(15) function eprimo(x)
implicit none
integer :: div                              !Variável que será o divisor dos testes
integer,intent(in) :: x                     !Valor de entrada fornecida pelo usuário

    eprimo = 'É primo'

    if ( x == 2) then                           !Condição que indicar que 2 é primo
        eprimo = 'É primo'
    else if ( mod(x,2) == 0) then               !Condição que indica que números pares (exceção do 2) não são primos
        eprimo = 'Não é primo'
    else
        do div = 3, int(sqrt(real(x))), 2       !If que testa a divisibilidade do valor de entrada
            if ( MOD(x,div) == 0.) then 
            eprimo = 'Não é primo'
            end if
        end do
    end if

end function
!
!
!
program Chamando_primo
    
implicit none
integer :: x                                    !Valor que o usuário irá fornecer 
character(15) :: eprimo                         !Função que irá indicar se é primo ou não
character(15) :: final                          !Variável que irá receber o valor da função

    write(*,*) 'Qual o valor que você quer saber?'
    read(*,*) x

        final = eprimo(x)

    write(*,*) 'o número',x,final

end program