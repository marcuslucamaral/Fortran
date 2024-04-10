program dipolo_eletrico
  implicit none
  real, allocatable :: ud(:), ub(:), x(:)
  real :: h, a, r, r1, r2, c, qe
  real, parameter :: pi=3.14159265358979323846
  real, parameter :: e0=8.8541878e-12
  integer :: i, j, n, m

  open(100,file='PotencialEletrico.dat',status='replace',action='write')
  
  write(*,*)'Quantos pontos no semi-eixo positivo x:'
  read(*,*)n

  m = 8*n+1

  allocate(ud(m),ub(m),x(m))

  h = .2
  a = 0.5
  qe = 1.

  do i=1,m

     x(i)=-n-0.25 + i/4.

     r = sqrt((x(i)**2) + (h**2))
     r1 = sqrt(((x(i)-a)**2) + (h**2))
     r2 = sqrt(((x(i)+a)**2) + (h**2))

     c = (1/(4.*pi*e0))

     ub(i) = ((c)*((-qe)/(r1))) + ((c)*((qe)/(r2)))
     ud(i) = (c)*((-2.*a*qe)/(r**2))*(x(i)/r)
  end do

  do j=1,m
     write(100,*)x(j),ud(j),ub(j)
  end do

  close(100)
  deallocate(ud,ub,x)
     
end program dipolo_eletrico
