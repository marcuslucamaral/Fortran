!===========================================
!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!================
program trajetoria
    implicit none
    real, parameter :: g = 9.8      !Valor da Gravidade 
    real :: h, v0, ang_rad, t, x    !h = altura, v0 = velocidade inicial, ang_rad = variavel que vai receber a função gr2rad, t = tempo até chegar
    integer :: a0


    !Chamada das entradas fornecidas pelo usuário
    write(*,*) 'Entre com os valores da altura em metros'
    read(*,*) h
    write(*,*) 'Agora digite o valor da velocidade de lançamento em m/s'
    read(*,*) v0
    write(*,*) 'Por fim digite o ângulo de lançamento (Atente-se que deve ser entre 0 e 90º)'
    read (*,*) a0

    !Looping para verificar se o ângulo está dentro dos parâmetros
    Do                                    
        if (a0 >= 0 .and. a0 <= 90) exit
        write(*,*) 'O valor não está dentro dos limites, por favor, digite um novo valor'
        read(*,*) a0
    end do      

    !Chama a função que irá converter graus para radianos
    ang_rad = gr2rad(a0)
    write(*,*) 'a conversão para radianos é',ang_rad

    !Chama a rotina que irá calcular o resultado da equação de segundo grau
    call eq2g (g, -(2 * v0 * sin(ang_rad)), -(2*h), t)


    write(*,*) 'O tempo para o impacto do projétil é de',t,'segundos.'

    x = v0 * cos(ang_rad) * t

    write(*,*) 'E a distância até o ponto de impacto é de',x,'metros.'
    
!-------------------------------------------------------------------------------------------------------------!
    CONTAINS
    
        real function gr2rad(a0)
            !Função que irá converter o ângulo em graus fornecido pelo usuário e converter para radianos
            implicit none
            integer :: a0
            real, parameter :: pi = 3.141592

            gr2rad = a0 * (pi / 180)
        end function

        subroutine eq2g (A, B, C, t)
            implicit none
            real, intent(in) :: A
            real, intent(in) :: B, C
            real, intent(out) :: t
            real :: delta, raiz1, raiz2

            delta = (B**2) - 4 * A * C
    
            raiz1 = (-B - sqrt(delta)) / (2 * A)
            raiz2 = (-B + sqrt(delta)) / (2 * A)

            if (raiz1 > raiz2) then
                t = raiz1
            else
                t = raiz2
            end if

        end subroutine

end program