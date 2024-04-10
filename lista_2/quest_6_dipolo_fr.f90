program dipolo
implicit none 
real, parameter :: pi = 3.141592653589
real, parameter :: e_o = 8.85e-12
real :: k
real, allocatable, dimension(:) :: x, u, v_positivo, v_negativo, v  ! Potencial, posição x 
real :: h ! Altura
real :: sup_sum, point_1, point_2, dist_x
integer :: i, n
real :: q ! carga
real :: a
a = 0.5
n = 1000 ! número de pontos
allocate(x(n+1), u(n+1), v_positivo(n+1), v_negativo(n+1), v(n+1))
! Cálculo dos pontos para x
point_1 = -30
point_2 = 30
dist_x = abs(point_1 - point_2)/n
sup_sum = point_1

do i = 1, n+1
    x(i) = sup_sum
    sup_sum = sup_sum + dist_x
end do

!do i = 1, n+1
!    write(*,*) x(i)
!end do

! cálculo  potencial dipolo 
!q = 1.6e-19
q = 1.
!k = 1./4*pi*e_o
k = 1.

h = 10. ! altura
a = 0.5 ! distância entre as cargas q+ e q-
do i = 1, n+1
    u(i) = ((-2.) * q * a * k * x(i)) / (((x(i))**2. + h**2.)**(3./2.)) ! expressão para o potencial do dipolo
    v_negativo(i) = ((-1.) * q * k)/ ((x(i)-a)**2 + h**2)**(1./2.)
    v_positivo(i) = (q * k)/ ((x(i)+a)**2 + h**2)**(1./2.)
    v(i) = v_positivo(i) + v_negativo(i)
end do

open(unit=22, file='quest_6.dat', status='replace', action='write')
do i = 1, n+1
    write(22,*) u(i), v(i), x(i)
end do



end program dipolo