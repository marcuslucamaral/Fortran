!
    ! Purpose:
    ! Uma imagem como esta pode ser construída a partir da distribuição de intensidades de três cores básicas: vermelho (red),
    !verde (green) e azul (blue). Um programa com ferramentas para exibir gráficos (MATLAB ou
    !OCTAVE) pode ler a informação sobre as cores em três matrizes e compor a imagem. Neste
    !problema você irá trabalhar com estas informações.
    !No arquivo fontes.dat está toda a informação para construir a imagem da foto. Na primeira
    !linha do arquivo estão dois números inteiros (Nx, Ny) que correspondem às quantidades de pixels
    !que formam a imagem, na horizontal e na vertical, nesta ordem. Em seguida vem uma única
    !coluna de dados com 3*Nx*Ny números. Estes números correspondem aos elementos de três
    !matrizes de tamanhos iguais, com as informações das três cores, na sequência red, green, blue.
    !Os valores estão organizados por coluna, de maneira que os primeiros Ny números formam a
    !primeira coluna da matriz Red, os próximos Ny valores formam a segunda coluna da mesma
    !matriz e assim por diante. Na sequência vem as colunas da matriz Green e depois as da Blue.
    !Seu programa deve dimensionar as matrizes a partir da leitura dos valores de Nx e Ny.
    !Você deve construir um programa para ler a informação contida no arquivo fontes.dat e a partir
    !dela montar três matrizes para serem importadas para o MATLAB ou OCTAVE para exibir
    !a imagem. Salve as matrizes em três arquivos chamados Red.dat, Green.dat e Blue.dat.
    !Depois, para visualizar a imagem final, simplesmente use o script monta_imagem.m dentro da
    !mesma pasta onde salvou os arquivos.
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
