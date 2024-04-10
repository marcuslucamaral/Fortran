!
    ! Purpose:
    ! Monta Matriz a partir de vetores
    ! 
    ! 
    !
    ! Record of revisions:
    ! Date Programmer Description of change
    ! ======== ============================= =============
    ! 21/05/23 Marcus Lucas Amaral do Amaral Original code

!================================================================================
   
   program monta_matrix
    implicit none

    integer:: Nx, Ny
    real,allocatable:: N_in(:) !vetor de entrada
    real, allocatable :: red(:,:),green(:,:),blue(:,:) !matriz de entrada
    
    integer:: i,j,k,l,m

    open(101, file='fontes.dat',status='old',action='read')

    read(101,*)Nx,Ny

    allocate(n_in(3*Nx*Ny))

    do i=1, 3*Nx*Ny
        read(101,*)N_in(i) 
    end do    

    allocate(Red(Ny, Nx), Green(Ny, Nx), Blue(Ny, Nx))


   ! Atribuir os dados às matrizes Red, Green e Blue
    k = 1
    do m = 1, Nx
        do l = 1, Ny
            Red(l, m) = N_in(k)
            Green(l, m) = N_in(k+Nx*Ny)
            Blue(l, m) = N_in(k + 2*Nx*Ny)
            k = k + 1
        end do
    end do 

  !  k = 1
   ! do m = 1, Nx
  !      do l = 1, Ny
   !         Red(l, m) = N_in(k)
    !        Green(l, m) = N_in(k + Ny)
    !       Blue(l, m) = N_in(k + 2*Ny)
     !       k = k + 1
      !  end do
    !end do 

    ! Criar dados red
    open(102, file='Red.dat',status='replace',action='write')

   ! !do i = 1, Ny
    !!       write(102, *) Red(i, :) 
   !!  end do

     do i = 1, Ny
        
           write(102, *) (Red(i,j),j=1,Nx)!do implicito 
        
     end do

      ! Criar dados green
    open(103, file='Green.dat',status='replace',action='write')

    do i = 1, Ny
       ! do j = 1, Nx
           write(103, *) green(i, :)
       ! end do
     end do

      ! Criar dados blue
    open(104, file='Blue.dat',status='replace',action='write')

    do i = 1, Ny
        !do j = 1, Nx
           write(104, *) blue(i, :)
        !end do
     end do

     write(*,*)'Terminou a criacao dos dador:red,green,blue'
     close(101)
     close(102)
     close(103)
     close(104)
   end program monta_matrix