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



subroutine Retrosubst(N,U,b,x)!matriz U, vetor b,vetor de saida x
    !
    !A rotina pressupoe que possui a forma certa de triangular superior
    implicit none
    integer, intent(in):: N
    real, intent(in):: U(N,N), b(n)
    Real,intent(out):: X(N)
    integer:: i, j
    real:: soma
    
    x(N) = b(N)/U(N,N)

    do i= N-1,1,-1
        soma = 0.
        do j=i+1,N
            soma = soma + U(i,j)*x(j)
        end do
        x(i) = (b(i)-soma)/U(i,i)
    end do



end subroutine Retrosubst 

end module Alin
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Program test_matrizes
Use Alin
implicit none
!integer, parameter :: M = 4
integer, parameter :: N = 30
Real :: A(N,N),x(N), b(N),f(N)
real::t(N),t0,tf,dt 
integer :: i,j
real:: t1, t2

!x = [1., 1., 1.]

!A(1,:) = [1., 2., 4.]
!A(2,:) = [0., 3., 5.]
!A(3,:) = [0., 0., 6.]
!b = [17.,21.,18.]



A = 0. !significa que zero a matriz toda

!criaçao de valores aleatorios na triangular superior
do i= 1, n
    do j = i, N
        call random_number(A(i,j))
    end do
end do

t0 = 0.
tf = 6.283185307179586

dt = (tf-t0)/(N-1)

do i = 1,N 
    t(i) = t0 + (i-1)*dt
    f(i) = sin(t(i))
end do


call Prod_Mat_Vet(N,N,A,f,b)

call Retrosubst(N,A, b,X)

open(101,file='Solucao.dat',status='replace')

    do i=1,N
        write(101,*) t(i),f(i),x(i)
    end do


    !call Prod_Mat_Vet(M, N, A, x, y)


!do i = 1, N
   ! write(*,*) x
!end do


end Program

! Se colocar M maior do que a dimensão atribuida a A ele vai acessa a organização pro coluna em ordem diferente.
! coloque na chamada o valor de M maior que o declarado na A... pra testar.

! Escreva a subrotina para calcular o produto matricial entre duas matrizes(dever de casa).
! C= A B 

! Próxima aula esquever uma subrotina para a solução da equação de laplace em um domínio 2d retangular.