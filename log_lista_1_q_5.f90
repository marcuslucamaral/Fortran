!===========================================
!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!===========================================
!

real function log_base_a (base,x)


implicit none
real, intent(in):: base,x

log_base_a = log10(x)/log10(base)

end function log_base_a


program test_logaritimo
    implicit none
    real:: xis, b, log_base_a, lg

    write(*,*)'Entre com um numero x qualquer e uma base a qualquer'
    read(*,*) xis, b
        lg = log_base_a(b,xis)

        write(*,*)'O log do numero x de base b é:', lg
end program test_logaritimo