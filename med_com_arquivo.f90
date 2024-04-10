program calcula_media
implicit none
integer:: i
integer:: N
real:: x, soma, media

open(101, file='dados.dat',status='old',action='read')

read(101,*) N
soma = 0.
do i =1,N
    read(101,*) x
    soma = soma + x

end do

close(101)
media = soma/n
write(*,*)'media',media
end program calcula_media