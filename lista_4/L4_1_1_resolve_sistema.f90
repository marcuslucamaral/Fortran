Module opera_matrizes
    !============================================================================================================
    ! Purpose:
    ! esse módulo contem operaçoes algebricas com vetores
! Record of revisions:
!  Date             Programmer             Description of change
! ======== =============================      =============
! 09/07/23   Marcus Lucas Amaral do Amaral     Original code
!===========================================================================
! 1 - produto entre matriz e vetor.
!============================================================================================================
    implicit none
    contains
    !============================================================================================================
    ! rotina que faz o produto entre matriz e vetor
    subroutine prod_mat_vet(matrix,vector, vector_out,n,m)
    implicit none
    !============================================================================================================
    ! declaração de variáveis
    integer, intent(in) :: n
    integer, intent(in) :: m
    real(8), intent(in), dimension(n,m) :: matrix
    real(8), intent(in), dimension(n) :: vector
    real(8), intent(out), dimension(n) :: vector_out
    integer :: i
    
    ! Produto entre matriz e vetor
    do i = 1,n
        vector_out(i) = dot_product(matrix(i,:), vector)
    end do
    
    end subroutine prod_mat_vet
    
    end module opera_matrizes
    !============================================================================================================
    
    Module Solve_system_mod
    !============================================================================================================
    ! The purpose: Módulo resolve um conjunto de sistemas
    ! 1 - Retrosubstituição: Quando tem-se uma matriz triangular superior
    ! 2 - Eliminação Gaussiana: Usa quando não se tem a matriz no formato triangular superior
    ! 3 - Subrotina de Verificação de matriz singular: verifica se o maior número na diagonal principal é menor que e (número pequeno)
    !============================================================================================================
    implicit none 
    contains
    !============================================================================================================
    ! Subrotina de retrosubstituição
    subroutine retrosubst(matrix_u, x, b, n)
    implicit none
    !============================================================================================================
    ! The variables
    integer, intent(in) :: n ! the dimension of the arrays
    real(8), intent(in) :: matrix_u(n,n) ! the matrix that is upper triangular
    real(8), intent(in) :: b(n) ! the vector b
    real(8), intent(out) :: x(n) ! the solution
    real(8) :: sup_sum
    integer :: i, j 
    !============================================================================================================
    ! The loop to calcule the solution of x
    x(n) = b(n) / matrix_u(n,n)
    
    do i = n-1, 1, -1
        sup_sum = 0.
        do j = i+1, n
        sup_sum = sup_sum + matrix_u(i,j) * x(j)
        end do
        !x(i) = (1./matrix_u(i,i)) * (b(i) - sup_sum)
        x(i) = (b(i) - sup_sum)/matrix_u(i,i)
    end do	
        
    end subroutine retrosubst
    !============================================================================================================
    ! Eliminação gaussiana
    subroutine elimination_gaussian(m,n,a,b)
    implicit none
    ! Declaração de variaveis
    integer, intent(in) :: m, n
    real(8), intent(inout) :: a(m,n)
    real(8), intent(inout) :: b(n)
    real(8) :: pivo, aux_m
    integer :: i, j, k, Ip
    real(8) :: temp(n)
    real(8) :: btemp
    real(8) :: mult_det
    real(8), parameter :: eps = 1.e-6
    ! loop da eliminação gaussiana
    do k = 1, n - 1
    ! Faz o pivoteamento parcial
        pivo = abs(a(k,k))
        Ip = k
        do i = k + 1, n
            if (abs(a(i,k)) > pivo) then
            pivo = a(i,k)
            Ip = i
            end if
        end do
    ! Change the position of the row into the old pivot and the new pivot in the matrix and the vector b.
        temp(:) = a(k,:)
        a(k,:) = a(Ip,:)
        a(Ip,:) = temp(:)
        btemp = b(k)
        b(k) = b(Ip)
        b(Ip) = btemp
        do i = k + 1, n
            aux_m = a(i,k) / a(k,k)
                ! Proposta do loop de j = k+1, n - economizar as operações de eliminação, pois a rotina não acessa abaixo da diagonal (a eliminação garante uma triangular superior, considerando que a matrix não é singular.).
                do j = 1, n
                    a(i,j) = a(i,j) - aux_m * a(k,j)
                end do
            b(i) = b(i) - aux_m * b(k)
        end do
    end do
    ! this is the test by the determinant of the matrix.
    ! checking if the matrix is singular 
    !mult_det = a(1,1)
    !do k = 2, n
    !	mult_det = mult_det * a(k,k)
    !end do
    !	if (abs(mult_det) < eps) then
    !	write(*,*) 'A matriz é singular'
    !	stop
    !	else
    !	write(*,*) 'A matriz não é singular'
    !	end if
    end subroutine elimination_gaussian
    !============================================================================================================
    !Verify_subroutine_singular
    subroutine Verify_subroutine_singular(n,a)
    integer, intent(in) :: n
    real(8), intent(inout) :: a(n,n)
    real(8) :: temp, aux
    real(8), parameter :: e= 1.e-5
    integer :: i, j
    
    temp = a(1,1)
    do i = 1, n
        if (a(i,i) > temp) then
        temp = a(i,i)
        end if
    end do 
    ! Verify if the matriz is almost to be singular...
    if (temp < e) then
        aux = temp
        write(*,*) 'A matriz é singular'
    else
        aux = temp
        write(*,*) 'A matriz não é singular'
    end if
    
    end subroutine verify_subroutine_singular
    
    end module solve_system_mod
    !============================================================================================================
    
    Program Elim_test
    !============================================================================================================
    ! Programmer : Marcus Lucas Amaral do Amaral 
    ! The purpose: This program create a problem system Ax = b And use the modules: Opera_matrizes 
    ! and solve_system_mod to aplicate the gaussian elimination, the pivoting and check if the matrix is singular.
    !============================================================================================================
    use opera_matrizes
    use Solve_system_mod
    implicit none
    integer, parameter :: N = 100
    Real(8) :: A(N,N),x(N), b(N),f(N)
    real(8)::t(N),t0,tf,dt 
    integer :: i,j
    real(8) :: t1, t2
    
    ! Create the matrix to the system
    call random_number(A)
    
    call Verify_subroutine_singular(n,a)
    
    t0 = 0.d0
    !tf = 6.283185307179586
    tf = 6.283185307179586d0
    dt = (tf-t0)/(N-1.d0)
    
    do i = 1,N 
        t(i) = t0 + (i-1.d0)*dt
        f(i) = sin(t(i))
    end do
    
    ! Creating the vector b from a matrix Af=b, f is supposed to be the solution 'x' of our system.
    call prod_mat_vet(A,f,b,N,N)
    
    ! Apply the gaussian elimination, the pivoting tecnique and the checking if the matrix is singular.
    
    call elimination_gaussian(N,N,A,b)
    
    ! Solve the system by using the back_substitution ('retro_subs')
    
    call retrosubst(A, x, b, n)
    
    open(101,file='Solution_quest1.dat',status='replace')
    write(*,*) 'A solução está na pasta ','"solution_quest1.dat"'
    write(*,*) 'Pode platar no OCTAVE pelo algoritimo','"plot_matrizes_solucao.m"'
    do i=1, N
        write(101,*) t(i), f(i), x(i)
    end do
    
    
    
    end program Elim_test
    