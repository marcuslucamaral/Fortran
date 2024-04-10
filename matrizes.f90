Module Alin
implicit none

contains

subroutine Prod_Mat_Vet(M,N,A,x,y)
implicit none
! ...
integer, intent(in) :: M, N
real, intent(in) :: A(M,N), X(N) 
real, intent(out) :: y(M)
integer :: i 

do i = 1, M
    y(i) = Dot_Product( A(i,:), x ) ! no A(i,:) equivale a A(i, 1:N)
end do 


end subroutine Prod_Mat_Vet

end module Alin
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Program test_matrizes
Use Alin
implicit none
integer, parameter :: M = 4
integer, parameter :: N = 3
Real :: A(M,N), x(N), y(M)
integer :: i
x = [1., 1., 1.]

A(1,:) = [1., 2., 3.]
A(2,:) = [2., 1., 2.]
A(3,:) = [1., 2., 1.]
A(4,:) = [1., 1., 1.]
call Prod_Mat_Vet(M, N, A, x, y)


do i = 1, M
    write(*,*) y(i)
end do


end Program

! Se colocar M maior do que a dimensão atribuida a A ele vai acessa a organização pro coluna em ordem diferente.
! coloque na chamada o valor de M maior que o declarado na A... pra testar.

! Escreva a subrotina para calcular o produto matricial entre duas matrizes(dever de casa).
! C= A B 

! Próxima aula esquever uma subrotina para a solução da equação de laplace em um domínio 2d retangular.