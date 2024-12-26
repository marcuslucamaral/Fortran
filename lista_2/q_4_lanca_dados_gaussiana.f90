!===================================================================================
    ! Purpose:
    !
    ! Se você lançar um dado muitas vezes, observará uma sequência de números aleatórios entre 1 e 6
    !com distribuição uniforme, ou seja, todo número tem igual probabilidade (16,667 %) de aparecer
    !em cada lançamento. Já no lançamento de dois dados, os números possíveis são de 2 a 12, agora
    !com probabilidades diferentes para cada número,
    !Com mais dados sendo lançados, as probabilidades passam a variar de maneira não linear, de
    !modo que quanto maior o número de dados em um lançamento, mais a curva das probabilidades
    !se assemelha a uma gaussiana.
    !Sua tarefa nesta questão é escrever um programa para verificar este comportamento:
    !1. Escreva uma subrotina que simule o lançamento de N dados. Para dados de 6 faces, os
    !resultados possíveis são os números de N até 6N.
    !2. Execute um grande número de lançamentos, registrando o número de vezes que cada número
    !foi obtido, e ao final calcule a probabilidade atingida por cada valor.
    !3. Salve os resultados num arquivo para visualizar o gráfico.
    !Como você faria para determinar se a distribuição é uma gaussiana verdadeira ou apenas apro-
    !ximadamente?
    !
    !
    ! Record of revisions:
    ! Date                                    Programmer                  Description of change
    ! ========                       =============================             =============
    ! 22/05/23                               Marcus Amaral                      Original code

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
