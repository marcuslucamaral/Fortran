!
    ! Purpose:
    ! Monta Matriz a partir de vetores, tranforma em 3 matrizes de cada cor
    !dimui o pixel pela metada, a parti da media de 4 valores ao redor, gera
    !a imagem com menor qualidade 
    ! 
    !
    ! Record of revisions:
    ! Date_begin/end_date Programmer Description of change
    ! ======== ============================= =============
    ! 24/05/23 Marcus Lucas Amaral do Amaral Original code
    ! 26/05/23
!================================================================================
   
   program monta_matrix_menor_qualidade_beni
    implicit none

    integer:: Nx, Ny
    real,allocatable:: N_in(:) !vetor de entrada
    real, allocatable :: R(:,:),G(:,:),B(:,:) !matriz de entrada
    real, allocatable :: R2(:,:),G2(:,:),B2(:,:) !matriz de saida
    integer:: i,j,k,l,m
    

    open(101, file='Beni.dat',status='old',action='read')

    read(101,*)Nx,Ny

    allocate(n_in(3*Nx*Ny))

    do i=1, 3*Nx*Ny
        read(101,*)N_in(i) 
    end do    

    allocate(R(Ny, Nx), G(Ny, Nx), B(Ny, Nx))


   ! Atribuir os dados às matrizes Red, Green e Blue
    k = 1
    do m = 1, Nx
        do l = 1, Ny
            R(l, m) = N_in(k)
            G(l, m) = N_in(k+Nx*Ny)
            B(l, m) = N_in(k + 2*Nx*Ny)
            k = k + 1
        end do
    end do 
    ! Criar dados red
    open(102, file='Red_beni.dat',status='replace',action='write')

    do i = 1, Ny
        !do j = 1, Nx
           write(102, *) R(i,:)
        !end do
     end do

      ! Criar dados green
    open(103, file='Green_beni.dat',status='replace',action='write')

    do i = 1, Ny
       ! do j = 1, Nx
           write(103, *) G(i, :)
       ! end do
     end do

      ! Criar dados blue
    open(104, file='Blue_beni.dat',status='replace',action='write')

    do i = 1, Ny
        !do j = 1, Nx
           write(104, *) B(i, :)
        !end do
     end do

     write(*,*)'Terminou a criacao dos dador:red_beni,green_beni,blue_beni'
     close(101)
     close(102)
     close(103)
     close(104)
!=================================================================
allocate(R2(Ny/2,Nx/2),G2(Ny/2,Nx/2),B2(Ny/2,Nx/2))

! criação da matriz Red2 com resoluçao menor

open(105, file='Red2_beni.dat',status='replace',action='write')

     
    do j = 1, Ny/2!Nx=2048
        do i=1,Nx/2!Ny=1152
             
     

            R2(i,j)=(R(2*i-1,2*j-1)+R(2*i,2*j-1)+R(2*i-1,2*j)+R(2*i,2*j))/4.
            
            !R2(1,1)= R(1,1)+R(1,2)+R(2,1)+R(2,2)
            !R2(2,1)= R(3,1)+R(3,2)+R(4,1)+R(4,2) 
            !R2(3,1)= R(5,1)+R(5,2)+R(6,1)+R(6,2)

     !R2(1024,1)= R(2047,1)+R(2047,2)+R(2048,1)+R(2048,2)
            
        end do
    end do
!=====================================================
    do i=1,Nx/2
    write(105,*)R2(:,i)
    end do
!=========================================================================
    ! criação da matriz green2 com resoluçao menor

open(106, file='Green2_beni.dat',status='replace',action='write')

do j = 1, Ny/2
    do i=1,Nx/2!ny=1024
         !j=1:Ny-1:2
 

        G2(i,j)= (G(2*i-1,2*j-1)+g(2*i,2*j-1)+G(2*i-1,2*j)+G(2*i,2*j))/4.
        !R2(1,1)= R(1,1)+R(1,2)+R(2,1)+R(2,2)
        !R2(2,1)= R(3,1)+R(3,2)+R(4,1)+R(4,2) 
        !R2(3,1)= R(5,1)+R(5,2)+R(6,1)+R(6,2)

 !R2(1024,1)= R(2047,1)+R(2047,2)+R(2048,1)+R(2048,2)
        
    end do
end do
!=====================================================
do i=1,Nx/2
    write(106,*)G2(:,i)
    end do
!=========================================================================

!=====================================================================
! criação da matriz Blue2 com resoluçao menor

open(107, file='Blue2_beni.dat',status='replace',action='write')

do j = 1, Ny/2
    do i=1,Nx/2!ny=1024
         !j=1:Ny-1:2
 

        B2(i,j)=(B(2*i-1,2*j-1)+B(2*i,2*j-1)+B(2*i-1,2*j)+B(2*i,2*j))/4.
        !R2(1,1)= R(1,1)+R(1,2)+R(2,1)+R(2,2)
        !R2(2,1)= R(3,1)+R(3,2)+R(4,1)+R(4,2) 
        !R2(3,1)= R(5,1)+R(5,2)+R(6,1)+R(6,2)

 !R2(1024,1)= R(2047,1)+R(2047,2)+R(2048,1)+R(2048,2)
        
    end do
end do    
!=====================================================
do i=1,Nx/2
    write(107,*)B2(:,i)
    end do
!=========================================================================

    Close(105)
    Close(106)
    Close(107)

    write(*,*)'Termino da criaçao das matrizes com baixa resolucao'
end program monta_matrix_menor_qualidade_beni