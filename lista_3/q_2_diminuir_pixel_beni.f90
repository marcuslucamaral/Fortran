!===============================================================================================================================
    ! Purpose:
    !
    ! No arquivo Beni.dat está toda a informação para construir a imagem abaixo, organizada no
    !arquivo exatamente da mesma maneira que foi descrita na questão 1.
    !A imagem tem 2.359.296 pixels dispostos em 1.152 linhas e 2.048 colunas, o que resulta num
    !arquivo ASCII de 127,4 MB. O objetivo desta tarefa é reduzir o número de pixels da imagem, de
    !maneira a gerar matrizes menores. Uma maneira simples de criar uma imagem com um quarto
    !do número de pixels (perdendo bastante informação!) é construir matrizes das intensidades de
    !cores de modo que cada pixel seja a média de 4 pixels da imagem original. Por exemplo, se
    !a imagem tivesse apenas 4 por 8 pixels, cada matriz reduzida seria 2 por 4, sendo que cada
    !elemento desta seria a média de 4 elementos da matriz original: 
    !Para gerar as matrizes reduzidas, você deve construir um programa para realizar os seguintes
    !passos:
    !1. Ler a informação contida no arquivo Beni.dat e a partir dela montar três matrizes, R, G
    !e B, com o tamanho original;
    !2. Dimensionar outras três matrizes (R2, G2, B2) com metade do número de linhas e metade
    !do número de colunas das matrizes originais;
    !3. Calcular o valor de cada elemento das matrizes reduzidas como a média dos valores de 4
    !elementos adjacentes das matrizes originais;
    !4. Salvar as matrizes em seis arquivos chamados Red.dat, Green.dat, Blue.dat, com os
    
    !valores originais, e Red2.dat, Green2.dat, Blue2.dat, com os valores das matrizes re-
    !duzidas.
    
    !Você pode considerar que os números de linhas e de colunas são sempre pares.
    !Ao final, para visualizar e comparar as imagens use o script monta_imagem_Beni.m na mesma
    !pasta onde estão salvas os arquivos com as matrizes. Verifique as diferenças nas imagens quando
    !você dá (zoom) nas duas. Verifique também as diferenças nos tamanhos dos arquivos com as
    !matrizes originais e as reduzidas.
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
