Integer function lanca_dado()
implicit none
Real:: x

call random_number(x)
lanca_dado = INT( 6*x + 1. )

end function lanca_dado
!!!!!!!!!!!!!!!!!!!!!!!!!!
Program aleatorios
implicit none
integer:: lanca_dado
Integer:: F1, F2, F3, F4, F5, F6
integer:: N, i, F

F1 = 0
F2 = 0
F3 = 0
F4 = 0
F5 = 0
F6 = 0
write(*,*) 'N?'
read(*,*) N

Do i = 1, N
    F = lanca_dado()
    If (F == 1) then
        F1 = F1 + 1
    else If (F == 2) then
        F2 = F2 + 1
    else If (F == 3) then
        F3 = F3 + 1
    else If (F == 4) then
        F4 = F4 + 1
    else If (F == 5) then
        F5 = F5 + 1
    else
        F6 = F6 + 1
    end if
end do

write(*,*) 'P1:', 100*(F1/real(N))
write(*,*) 'P2:', 100*(F2/real(N))
write(*,*) 'P3:', 100*(F3/real(N))
write(*,*) 'P4:', 100*(F4/real(N))
write(*,*) 'P5:', 100*(F5/real(N))
write(*,*) 'P6:', 100*(F6/real(N))

end program