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
