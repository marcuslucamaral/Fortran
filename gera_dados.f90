program gera_data
implicit none

integer:: N, i
real:: x


write(*,*) ' Entre com a quantidade de dados:'

read(*,*) N

open(101, file='data.dat',status='replace',action='write')
write(101,*) N

do i = 1,N
    call random_number(x) ! ele gera numeros entre fechado no 0 e aberto no 1
        x = 2*(x-0.5) ! muda-se o intervalo entre -1 a 1
        write(101,*) x
end do


end program gera_data