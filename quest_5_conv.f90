subroutine conver_grau(a, m, seg, g)
implicit none 
real, intent(in) :: a
integer, intent(out) :: g, m
real, intent(out) :: seg
real :: temp_g, tem_seg, temp_m, temp_m1, temp
real, parameter :: pi = 3.14159265

! Cálculo do grau, minuto e segundo
! cálculo do grau
temp_g = a * (180./pi)
write(*,*) temp_g
g = temp_g
! cálculo do minuto
temp_m = temp_g - g
temp_m1 = temp_m * 60.
write(*,*) temp_m1
m = temp_m1
! cálculo do segundo
temp = temp_m1 - m
write(*,*) temp
seg = temp * 60.
end subroutine
