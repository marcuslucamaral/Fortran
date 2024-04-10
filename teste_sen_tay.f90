program taylor_sine
  implicit none

  integer, parameter :: dp = selected_real_kind(15, 300)  ! double precision 
  integer :: i, n
  real(dp) :: x, term, sine, fact, g
  real(8), parameter :: pi = 3.1415926535
  write(*,*) 'Enter the value of x (in graus):'
  read(*,*) g
  x = g * pi / 180.	
  sine = 0.0_dp
  term = x
  fact = 1.0_dp

  do i = 1, 10   ! Calculate the first 10 terms of the series
     sine = sine + term
     term = -term * x * x / ((2*i) * (2*i+1))
  end do

  write(*,*) 'The value of sin(',x,') using the first 10 terms of the Taylor series is: ',sine
  write(*,*) 'The number of iterations', i
  write(*,*) 'the original value of sen by the implicit function', dsin(x)
end program taylor_sine

