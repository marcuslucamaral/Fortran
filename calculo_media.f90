program media
    !!programa que calcula media de N valores 
    !! 

implicit none 
Integer:: N ! N é numero de medidas
Integer:: i ! i unidade loop
real:: soma, x, md

    write(*,*) 'Quantas medidas N'
    read(*,*) N

    soma = 0.
do i = 1, N
    write(*,*) 'Digite o valore de medida',i,':'
    read(*,*) x
    soma = soma + x             !toda vez que acumular uma soma assim declare antes do do um valor inicial a soma soma=0.
end do
    md = soma/n    
write(*,*) 'media', md

end program media