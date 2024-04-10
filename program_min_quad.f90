program minimos_quad
    implicit none
    Real(8), allocatable :: x(:) !valores x de entrada
    Real(8), allocatable :: y(:) !valores y de entrada
    Real(8), allocatable :: x_zao(:,:) !matriz de x onde XTXc=XTy
    Real(8), allocatable :: c(:) !vetor de saída de coeficientes
    Real(8), allocatable :: XTX(:,:) !matriz x transposta x
    Real(8), allocatable :: XTY(:) ! vetor x tranposta y
    integer:: i, j!contadores
    Integer:: g,g1!g = grau do polinômio,
    Integer:: N! N = numero de pontos
    Real(8):: w, x_i,x_f! w = amplitude do ruído, x_i e x_f é o intervalo de x
    Real(8)::r 
!==========================================================================================
!Leitura dados entrada:

    open(101, file = 'dados_poli_output.dat', status = 'old', action = 'read')
    Read(101,*)N,g

    g1=g+1

    allocate(x_zao(N,g1),x(N),y(N),c(g1),XTX(g1,g1),XTY(g1))
    
    do i=1,N 
        read(101,*) x(i),y(i)
    end do 

    close(101)
!===================================================================================

!Monta Matriz X_zao
    x_zao(:,1) = 1.d0
    do j=2,g1 
        x_zao(:,j) = x**(j-1)
    end do  

!====================================================
!Constroi XtX
!operações   XTXc=XTy 
do j=1,g1
    do i=j, g1
        XTX(i,j) = Dot_product(x_zao(:,i),x_zao(:,j))
        XTX(j,i)=XTX(i,j)!esse é o x transposta xc
    end do
    XTY(j) = Dot_product(x_zao(:,j),y) !x transposta y
end do

!===================================================================
!Resolve sistema

call Fatora_LU(g1,XTX)
call Resolve(g1,XTX,XTY)


open(102, file = 'coeficientes.dat', status = 'replace', action = 'write')

!Saída 
    Do i = 1, g1
        write(102,*) XTY(i)
    end do

end program minimos_quad