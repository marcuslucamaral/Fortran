!================================================================
! Subrotina que simula o lancamento do dado
! ===============================================================
subroutine lanca(resultado)
  implicit none
  integer, intent(out) :: resultado
  real :: x
  
  call random_number(x)
  resultado = int(1. + (6.*x))

end subroutine lanca

!================================================================
! Programa Principal
!================================================================

program gaussiana
  implicit none
  integer :: nlances, ndados, i, j, k, valor, m
  integer, allocatable :: F(:)

  open(100,file='resultados.dat',status='replace',action='write')

  write(*,*)"Entre quantidade de lancamentos:"
  read(*,*)nlances
  write(*,*)"Entre quantidade de dados:"
  read(*,*)ndados

  allocate(F(ndados:6*ndados))

  F = 0
  do j=1,nlances
     m = 0
     do i=1,ndados
        call lanca(valor)
        m = m + valor
     end do
     F(m) = F(m) + 1
  end do

  do k=ndados,6*ndados
     write(100,*)k,100*(F(k)/real(nlances))
  end do
  
  close(100)
  deallocate(F)

end program gaussiana
