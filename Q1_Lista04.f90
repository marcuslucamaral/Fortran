program solve_linear_system
    use lista04

    implicit none
    
    integer,parameter:: N = 500
    real:: A(N,N), x(N), b(N), f(N)
    integer :: i, j, k, pivot_row
    real :: det
    real:: t(N), t0, tf, dt
    

    call random_number(A)


    t0 = 0.
    tf = 2.*pi
    dt = (tf-t0)/(N-1)

    do i = 1, N
        t(i) = t0 + (i-1)*dt
        f(i) = sin(t(i))

    end do

    call create_solution(N,N,A,f,b)
    
	Do i = 1, N
		if (A(i,i) == 0) then
			Write(*,*)'A matriz é singular'
			Stop
		End If
	End Do
    
    ! Perform Gaussian elimination with pivoting.

    call Elimina(N,A,b)
  
    
    ! Backsolve to find the solution vector x.
    
    do i = n - 1, 1, -1
      x(i) = (b(i) - dot_product(A(i, i + 1:), x(i + 1:))) / A(i, i)
    enddo
    
    ! Save the solution.
    open(42, file = 'solucao.dat', status = 'replace')

    do i = 1, N
        write(42,*)t(i), f(i), x(i)
    end do
    
end program solve_linear_system
        
