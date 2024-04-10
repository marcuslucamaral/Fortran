Integer function lanca_dado()!quando a function esta assim, nao precisa de entrada, apenas saida
implicit none
Real:: x

call random_number(x)
lanca_dado = INT( 6*x + 1. )

end function lanca_dado
!!!!!!!!!!!!!!!!!!!!!!!!!!
Program aleatorios
implicit none
integer:: lanca_dado
Integer:: F(6)
integer:: N, i, face

F = 0

write(*,*) 'N?'
read(*,*) N

Do i = 1, N
    Face = lanca_dado()
    F(Face)= F(Face) + 1
end do

write(*,*) 'P1:', 100*(F(1)/real(N))
write(*,*) 'P2:', 100*(F(2)/real(N))
write(*,*) 'P3:', 100*(F(3)/real(N))
write(*,*) 'P4:', 100*(F(4)/real(N))
write(*,*) 'P5:', 100*(F(5)/real(N))
write(*,*) 'P6:', 100*(F(6)/real(N))

end program