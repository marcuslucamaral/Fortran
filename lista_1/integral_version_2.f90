!================================================================================
 
    ! Purpose:
    !
    ! Calculo da integral por trapezios, nesse  se aumenta o numero de intervalos para perceber
    !a convergencia, aumenta a quanditade de intervalos  por 2x
    !
    ! Record of revisions:
    ! Date_begin/end_date                 Programmer                             Description of change
    ! ========                     =============================                      =============
    ! 16/05/2023                            Marcus Amaral                               Original code
    ! unknown
!================================================================================

module integracao
    implicit none
    real, parameter:: pi = 3.14159265 
    real, parameter:: e = 2.718281828
    contains

subroutine trapezios_0(Fun, a, b, N, S)
    implicit none
    Real:: Fun  !nao declarar o valor da fun como intent(in), intent(in) é para variavel  
    Real, intent(in):: a,b
    Integer, intent(in):: N
    Real, intent(out):: S
    Real:: h, x
    Integer:: i


    S = 0.
    h = (b-a)/N
    x = a

    do i = 1, N-1                   
        x = x + h                   !pode ser assim x = a + i*h , sem declarar antes x = a
        S = S + Fun(x)
    end do

    S = (2*S + Fun(a) + Fun(b))*h/2

    end subroutine trapezios_0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine trapezios(Fun, a, b, S2)
        implicit none
        Real:: Fun  !nao declarar o valor da fun como intent(in), intent(in) é para variavel  
        Real, intent(in):: a,b
        Integer:: N
        Real, intent(out):: S2
        real:: S1, soma_i, Soma_e
        Real:: h, x
        Integer:: i
        real:: eps ! é melhor pensar em calcular um erro S2-S1/S1, pois o erro pode ta na mesma ordem de grandeza de 1.e-5
        eps = 1.e-5
        N = 3.
        ! primeira estimativa de area
        soma_e = Fun(a) + Fun(b)
        h = (b-a)/N
        Soma_i = 0.
        x = a
    
        do i = 1, N-1                   
            x = x + h                   !pode ser assim x = a + i*h , sem declarar antes x = a
            Soma_i = Soma_i + Fun(x)
        end do
    
        S1 = (2*Soma_i + Soma_e)*h*0.5


        do 
            x = a - h*0.5
            do i=1, N
                x = x + h
                soma_i = soma_i + fun(x)
                
            end do

            h = h*0.5
            N = 2*N
            S2 = (2*Soma_i + Soma_e)*h*0.5

            if (abs((S2-S1)/S1)<eps) then 
                exit
            else
                S1=S2
            end if

        end do
    write(*,*) 'N:',N
        end subroutine trapezios




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    real function quadratica(x)
    implicit none
    real,intent(in):: x
    quadratica = x**2


end function quadratica
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real function seno(x)
implicit none
real,intent(in):: x
seno = sin(x)


end function seno

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real function hiperbole(x)
implicit none
real,intent(in):: x

hiperbole = 1./x


end function hiperbole

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module integracao


Program testa_trapezios
    use integracao
    implicit none
    real:: a, b , S
    integer:: N

    a = 0.
    b = 1.
    N = 20.

    call trapezios(quadratica, a,  b, S)

    write(*,*)'Integral de x^2 de 0 a 1:', S
    write(*,*)'Valor Verdadeiro:', 1./3.

    a = 0.
    b = pi
    n = 100
    
    call trapezios(seno, a, b, S)

    write(*,*)'Integral de Sen de 0 a pi:', S
    write(*,*)'Valor Verdadeiro:', 2.


    a = 1.
    b = e
    n = 100
    
    call trapezios(hiperbole, a, b, S)

    write(*,*)'Integral de hiperbole de 1 a e:', S
    write(*,*)'Valor Verdadeiro:', log(b)


end program testa_trapezios
