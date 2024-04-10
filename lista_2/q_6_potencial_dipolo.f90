  !
    ! Purpose:
    ! Esse programa calcula o potencial de um dipolo
    ! de um monopolo, salva valores em um arquivo para 
    ! plotar os valores do potencil de ambos em relaçao a h
    ! ao longo dos valores de x
    ! Record of revisions:
    ! Date              Programmer            Description of change
    ! ======== ============================= =============
    ! 27/05/23 Marcus Lucas Amaral do Amaral Original code

!================================================================================

program potencial_di_bi
implicit none  
real, parameter :: pi = 3.1415926536   ! pi
real, parameter :: q = 1.6e-19
real, parameter :: e_zero = 8.854e-12 !permissividade eletrica no vácuo
!integer, parameter:: N=100
real::a ! distancia carga centro 
real:: h !altura entre linha de medida e da carga
real:: k !constante
real:: p!momento de dipolo
real,dimension(-30:30):: x, Ub,Ud  !valores da coordenada
!real:: Ub(-30:30) !potencial do bipolo
!real:: Ud(-30:30)!potencial do dipolo
integer:: i
!==============================================================
h = 1.
a= 0.5
k = 1./(4*pi*e_zero)
p = -2*a*q!momento de dipolo

!Alocacao dos vetores
!allocate(x(N),Ud(N),Ub(N))

!Calculo dos valores de X,Potencial dipolo,Potencial bipolo

do i=-30,30
  x(i)=i
  Ud(i) = p*k*x(i)/sqrt((x(i)**2)+(h**2))**3
  Ub(i) = k*q*((1/sqrt((x(i)+a)**2+(h**2))-1/sqrt((x(i)-a)**2+(h**2))))
end do

! Salvando em um dado
open(101, file='q_6_plot_h_1.dat', status='replace',action='write')

do i=-30,30 
write(101,*)x(i),Ud(i),Ub(i)
end do

close(102)
write(*,*)'Termino da criacao dos dados'
end program potencial_di_bi