program taylor_cosine
  implicit none

  integer, parameter :: dp = selected_real_kind(15, 300)  ! double precision
  integer :: i, n
  real(dp) :: x, term, cosine, fact,  g
  real(8), parameter :: pi = 3.1415926535
  write(*,*) 'Enter the value of x (graus):'
  read(*,*) g
  x = g * pi / 180.
  cosine = 1.0_dp
  term = 1.0_dp
  fact = 1.0_dp

  do i = 1, 10  ! Calculate the first 10 terms of the series
  do 
     i = i + 1
     fact = real(fact,8) * (2*real(i,8)-1) * (2*real(i,8))
     term = -term * x * x / real(fact,8)
     cosine = cosine + term
  end do

  write(*,*) 'the aproximate of the taylor serie of the cos function',cosine
  write(*,*) 'the original function with intrinsic function', dcos(g)
  write(*,*) 'the number of iterations', i
end program taylor_cosine

