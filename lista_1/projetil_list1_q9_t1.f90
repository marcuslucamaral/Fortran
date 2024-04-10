!===========================================
!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!================


! The subroutine calculate the solution of the equation with grau two.
subroutine squared( A, B, C, Nr, R1, R2)
    
    ! This subroutine gives the solution of the equation Ax^2+ Bx + C = 0
    
    ! if
    implicit none
    Real, intent(in) :: A, B, C
    Real, intent(out) :: R1, R2
    Integer, intent(out) :: Nr
    real :: Delta
        
    Delta = (B**2) - (4*A*C)
    if (Delta < 0.) then
        Nr = 0
    else if (Delta > 0.) then
        Nr=2
        R1 = (-B -sqrt(Delta)) / (2 * A)
        R2 = (-B + sqrt(Delta)) / (2 * A)
        ! you can save the defoul calculate in a variable to avoit the operation that will problaby need to do the calculate again
    else 
        Nr=1
        R1=-B / (2*A)
        R1=R2
    end if
        
    end subroutine
    
    program tempos_da_escola
    implicit none
    real :: h_o, v_o, rad, degree
    real :: a, b, c
    real, parameter :: g = 9.8
    real, parameter :: pi = 3.1415926535
    real :: t_1, t_2, t, x
    integer :: N
    write(*,*) 'Escreva os valores para: velocidade e altura'
    read(*,*) v_o
    read(*,*) h_o
    
    write(*,*) 'Escreva o valor para o angulo:'
    read(*,*) degree
    
    do 
        if (degree < 0. .or. degree > 90) then
            write(*,*) 'Valor errado tente denovo: escreva um valor menor ou igual a 90 graus' 
            read(*,*) degree
        else
        exit
        end if
    end do
    
    rad = degree * ( pi/ 180. )
    
    a = g
    b = -2 * v_o * sin(rad)
    
    c = -2 * h_o 
    call squared( a, b, c, N, t_1, t_2)
    
        if( N == 1) then
            t = t_1
        else if ( N==2) then
            t = t_2
        else
            write(*,*) 'não tem solução'
            stop
        end if 
    ! Calculo da distância
    
    
    write(*,*) 'O tempo gasto até a posição do impacto:', t
    
    x = v_o * cos(rad) * t
    
    write(*,*) 'A posição de impacto (distância percorrida) x vale:', x
    end program
    
    
    
    
    
    
    