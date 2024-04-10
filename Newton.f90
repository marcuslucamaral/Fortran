!=======================================================================
! Universidade Federal Do Pará
! Aluno: José Frank Vanzeler Gonçalves
! Disciplina : Fortran
! Programa de pós-graduação em Geofísica
! Professor: Cícero Régis
! data:17/03/2023
! Código otimizado
!=======================================================================

Real function Fun(x)
implicit none
real, intent(in) :: x
!=======================================================================
!Fun = x**2 - 4.
!Fun = x**2 + 4 ! nunca converge.
!Fun = log(x) ! valores de x acima de 2.7 não convergem

!Fun = x**3 - 2.*x - 5 ! a raiz é  2.09455132
Fun = x**2 - 4*x - 7 ! a raiz é 5.31662


end function Fun
!=======================================================================
Real function Deriv(x)
implicit none
real, intent(in) :: x
real, parameter :: h = 0.001
real :: Fun
!Deriv = 2.*x
Deriv = (Fun(x+h) - Fun(x))/ h ! the general form with limite

!Deriv = 1./x ! existe um limite quando o valor é muito grande (x=10-exemplo)
end function Deriv
!=======================================================================
program newton
implicit none
real :: x
real, parameter :: eps= 1.e-5
real :: Fun 
real :: Deriv
integer :: N
real :: T
!=======================================================================
N=0
write(*,*) 'Qual o valor inicial x0?'
read(*,*) x

do 
    N = N + 1
    x = x - (fun(x)/Deriv(x))
    write(*,*) x, n
    if (abs(fun(x))<eps) exit ! tem a sugestão de testar a diferença entre o valor atual e o anterior
end do 
!=======================================================================

write(*,*) 'Raiz', x
!T = Deriv(x)
!write(*,*) T 

end program newton
