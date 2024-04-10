program teste_empilhamento
implicit none 
integer, parameter:: Nx=5!colunas
integer, parameter:: Ny=4!linhas
real:: R(Ny,Nx)!entrada
real::R2(Ny)!saida
real:: soma
!matriz de saida
integer:: i,j


R(1,:)=[1.,2.,2.,2.,2.]
R(2,:)=[2.,3.,3.,3.,3.]
R(3,:)=[3.,4.,4.,4.,4.]
R(4,:)=[4.,5.,5.,5.,5.]


soma = 0.
    
    do i = 2,Nx
    soma = soma + R(1,i)
   
    end do
    R2(1) = soma/(Nx-1)


soma = 0.

    do i = 2,Nx
    soma = soma + R(2,i)
   
    end do
    R2(2) = soma/(Nx-1)

!do i = 1,Nx-1 
!soma(R(i,1)) = R(i,1) + R(i+1,1)+R(i+2)
!n = n + 1

!soma = soma + R(i,j)




!do j=1,Ny
  !!  do i=1,5-1
!R2(i) = sum(R(i:5,j))
    !end do
!end do

!write(*,*)R
!do i = 1, Ny
!write(*,*)R(1,:)
    write(*,*)R2(1)
    write(*,*)R2(2)
!end do

end program teste_empilhamento