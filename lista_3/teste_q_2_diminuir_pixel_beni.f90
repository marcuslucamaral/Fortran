program teste_matrix_beni
    implicit none
    integer, parameter:: Nx=4!linhas
    integer, parameter:: Ny=8!coluna
    real:: R(Ny,Nx)!entrada
    real::R2(Ny/2,Nx/2)!saida
    !matriz de saida
    integer:: i,j
    

    R(1,:)=[1.,1.,2.,2.]
    R(2,:)=[1.,1.,2.,2.]
    R(3,:)=[3.,3.,4.,4.]
    R(4,:)=[3.,3.,4.,4.]
    R(5,:)=[5.,5.,6.,6.]
    R(6,:)=[5.,5.,6.,6.]
    R(7,:)=[7.,7.,8.,8.]
    R(8,:)=[7.,7.,8.,8.]

    
    do j = 1, Nx/2
        do i=1,Ny/2!ny=1024
             !j=1:Ny-1:2
     

            R2(i,j)= (R(2*i-1,2*j-1)+R(2*i,2*j-1)+R(2*i-1,2*j)+R(2*i,2*j))/4.
            !R2(1,1)= R(1,1)+R(1,2)+R(2,1)+R(2,2)
            !R2(2,1)= R(3,1)+R(3,2)+R(4,1)+R(4,2) 
            !R2(3,1)= R(5,1)+R(5,2)+R(6,1)+R(6,2)

     !R2(1024,1)= R(2047,1)+R(2047,2)+R(2048,1)+R(2048,2)
            
        end do
        end do

       ! do  j=1,Ny
         !   write(101,*)R2(:,j)
        !    end do

        do i = 1, Ny/2
            write(*,*)R2(i,:)
        end do
        


end program teste_matrix_beni
