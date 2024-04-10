PROGRAM Newton
    Implicit none 
    real :: x
    real, parameter :: eps = 1.e-5

    Write(*,*) 'Qual o ponto inicial?'
    Read(*,*) x

    Do
        x = x - ( fun(x) / deriv(x) )
        if ( abs(fun(x)) < eps ) exit
    End do

    write(*,*) 'Raiz: ', x
 !---------------------------------------------------------------------!
 CONTAINS
    Real FUNCTION fun(x)
        Implicit none
        real, intent(in) :: x

        fun = x**2 - 4
    end FUNCTION

    Real FUNCTION deriv(x)
        Implicit none
        real, intent(in) :: x

        deriv = 2 * x
    end FUNCTION

End PROGRAM