 real function deltag(dd,raio,z0,x0,x,dg)
  implicit none
  real, intent(in) :: dd, raio, z0, x, x0
  real, intent(out) :: dg
  real :: a, M
  real, parameter :: cg = 6.674e-11
  real, parameter :: pi=3.14159265358979323846

  M = (100)*(4.*pi*(raio**3.)*dd)/3.
  a = (x-x0)**2 + (z0)**2
  
  dg = (cg*M)*((z0)/(a**(3./2.)))

end function deltag

program campo_gravimetrico
  implicit none
  real :: deltag, dg
  integer :: i, nmedidas, nesferas, j
  real, allocatable :: x(:), cd(:), re(:), x0(:), z0(:), y(:,:)

  write(*,*)'Entre a quantidade de esferas:'
  read(*,*)nesferas
  write(*,*)'Entre a quantidade de posicoes para medidas no eixo x'
  read(*,*)nmedidas

  allocate(cd(nesferas))
  allocate(re(nesferas))
  allocate(z0(nesferas))
  allocate(x0(nesferas))
  allocate(x(nmedidas))
  allocate(y(nmedidas,nesferas))

  open(100,file='CampoGravimetrico_nesferas.dat',status='replace',action='write')

  do j=1,nesferas
     write(*,*)'Entre o valor de contraste de densidade',j
     read(*,*)cd(j)
     write(*,*)'Entre o valor do raio da esfera',j
     read(*,*)re(j)
     write(*,*)'Entre com a coordenada z0 do centro da esfera',j
     read(*,*)z0(j)
     write(*,*)'Entre com a coordenada x0 do centro da esfera',j
     read(*,*)x0(j)
  end do

  x=0
  y=0
  do i=1,nmedidas
     do j=1,nesferas
        x(i) = -50.+(((i-1.)/(nmedidas-1.))*100.)
        y(i,j) = y(i,j) + deltag(cd(j),re(j),z0(j),x0(j),x(i),dg)
     end do
     write(100,*)x(i),y(i,1)+y(i,2)+y(i,3)
  end do

  close(100)
  deallocate(x,cd,re,z0,y)
end program campo_gravimetrico
