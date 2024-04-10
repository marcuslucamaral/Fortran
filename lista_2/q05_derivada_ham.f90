subroutine calc_deriv(x1,y1,x2,y2,deriv)
  implicit none
  real, intent(in) :: x1,y1,x2,y2
  real, intent(out) :: deriv

  deriv = (y2-y1)/(x2-x1)

end subroutine calc_deriv

program derivada
  implicit none
  real, allocatable :: x(:),y(:)
  real :: deriv, xc, a, b
  integer :: erro, nlinhas, i


  open(100,file='Funcao.dat',status='old',action='read')
  open(200,file='Derivada_Numerica.dat',status='replace',action='write')

  nlinhas = 0
  do
     read(100,*,iostat=erro)a,b
     if (erro /= 0)exit
     nlinhas = nlinhas + 1
  end do

  rewind(100)
  allocate(x(nlinhas))
  allocate(y(nlinhas))

  do i = 1,nlinhas
     read(100,*,iostat=erro)x(i),y(i)
     if (erro /=0)exit
  end do

  do i=1,nlinhas-2
     call calc_deriv(x(i),y(i),x(i+1),y(i+1),deriv)
     xc = (x(i)+x(i+1))/2.
     write(200,*)xc,deriv
  end do

  deallocate(x)
  deallocate(y)
  close(100)
  close(200)
end program derivada

