  !
    ! Purpose:
    ! Esse programa possui uma subroina que calcula
    ! o lançamento de N dados, 6 faces cada, registra 
    ! o numero de eventos de cada numero e a probabilidade
    !de cada valor
    ! Record of revisions:
    ! Date              Programmer            Description of change
    ! ======== ============================= =============
    ! 22/05/23 Marcus Lucas Amaral do Amaral Original code

!================================================================================

Module dadinho
implicit none
contains

subroutine lanca_dado(lanca_data)!quando a function esta assim, nao precisa de entrada, apenas saida
implicit none
integer,intent(out)::lanca_data
Real:: x

call random_number(x)
lanca_data = INT( 6*x + 1. )

end subroutine

end module dadinho
!============================================================================

program testa_dados
  use dadinho
  implicit none
  
  integer :: N   ! Número de dados
  integer :: NL  ! Número de lançamentos
  integer:: i, j, k, l!contadores
  integer:: x!valor do dado
  integer,allocatable :: Result(:) ! Array para armazenar o número de vezes que cada número foi obtido
  !real, allocatable :: Prob(:)  ! Array para armazenar as probabilidades
  
 
  
  !entrada com valores de N e NL
  write(*,*) "Entre com valores de Numero de dados(N):"
  Read(*,*) N
  write(*,*) "Entre com valores de Numero lançamentos(NL):"
  read(*,*) NL

  allocate(Result(N:6*N))

  result = 0
  do j=1,Nl
     l = 0
     do i=1,N
        call lanca_dado(x)
        l = l + x
     end do
     Result(l) = Result(l) + 1
  end do

  ! Salva os resultados em um arquivo
  open(101, file='q_4_plot.dat', status='replace',action='write')
  do k=N,6*N
     write(101,*)k,100*(Result(k)/real(NL))
  end do
  
  
  
  close(101)
  
  Write(*,*)'Término da criaçao dos dados'
  
end program testa_dados