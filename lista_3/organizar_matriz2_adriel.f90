program organizar_matriz
implicit none
integer:: i, j
integer:: nx, ny
real, allocatable:: matriz (:)
real, allocatable, dimension(:,:):: red, green, blue
integer:: cont
character(len=13):: formato = '(1024F16.9)' ! Variável para armazenar o formato

open (55, file = 'fontes.dat', status = 'old',action = 'read')
read(55,*)nx, ny ! onde nx numero de colunas (eixo y) e ny numero de linhas (eixo x)

! j = 0
! do ! loop usado para contar a dimensão do arquivo (nesse caso, o numero de linhas)
!     read(55,*,iostat = i ) ! numero de linhas (eixo x)
!     if (i /= 0) exit
!     j = j + 1 ! para cada dado válido dentro dpo arquivo, 1 unidade é acrescido em j
    
! end do

allocate(matriz(3*nx*ny)) ! ou allocate(matriz(j)), caso o loop acima seja utilizado
read(55,*)matriz
allocate(red(ny,nx), green(ny,nx), blue(ny,nx))
!aux = size(matriz, dim=1)

!! vermelho 
cont=0
do j = 1, nx ! numero de colunas
    do i = 1, ny ! numero de linhas
        cont = cont + 1
        red(i,j) = matriz(cont)
    end do
    if (j == nx) exit
end do 
red = transpose(red)
open(66, file='red.dat', status='replace', action='write')
write(66,formato)red
!101 format (1024F18.9,x)


!! verde 
do j = 1, nx ! numero de colunas
    do i = 1, ny ! numero de linhas
        cont = cont + 1
        green(i,j) = matriz(cont)
    end do
end do 
green = transpose(green)
open(67, file='green.dat', status='replace', action='write')
write(67,formato)green


!! azul
do j = 1, nx ! numero de colunas
    do i = 1, ny ! numero de linhas
        cont = cont + 1
        blue(i,j) = matriz(cont)
    end do
end do 
blue = transpose(blue)
open(68, file='blue.dat', status='replace', action='write')
write(68,formato)blue

end program