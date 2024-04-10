program formatos_test
implicit none
integer:: A,B,C,d,f,g,h
real:: x, y, z
A = 12323
B = 45623
C = 78923
d= 534532
f= 3454
g=4535
h=4323
!x = 1.23456
!y = 1.234e-4
!z = X*y
write(*,200) A, B, C, d, f, g, h

!write(*,200) x, y, z
!200 format(3I5)   ! inteiro
!200 format (2F15.4) ! real
!200 format (2E15.4) ! potencia de 10
200 format(3(I6,2x))
end program formatos_test