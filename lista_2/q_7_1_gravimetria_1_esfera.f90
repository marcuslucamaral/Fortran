!Funcao que calcula o valor do campo gravitacional  
  real function func_grav(delta_rho,R,z_0,x)
  real, intent(in):: delta_rho
  real, intent(in) :: R
  real, intent(in) :: z_0
  real, intent(in) :: x
  real, parameter :: pi = 3.1415926536   ! pi
  real, parameter :: const_g = 6.674e-11

  func_grav = ((const_g)*(pi)*(4./3)*(delta_rho)*(z_0)*(R**3))/(sqrt((x**2)+(z_0**2))**3)

end function  func_grav
  
  
  
  
  
  !=============================================================================================
    ! Purpose:
    ! Esse programa calcula a gravidade em um ponto em superficie sobre influencia de uma esfera em 
    ! subsuperficie, vc entra com valor de contraste de densidade, raio da esfera, profundidade para 
    ! o centro da esfera, projecao em x e sai com valores do contraste de gravidade no ponto de medida
    ! x, esse programa lista em um arquivo os valores de gravidade em seu respectivo x
    ! Record of revisions:
    ! Date              Programmer            Description of change
    ! ======== ============================= =============
    ! 27/05/23 Marcus Lucas Amaral do Amaral Original code

!================================================================================

program gravidade_1_esfera
    implicit none  
    real:: func_grav
    real::R
    real::delta_rho
    real:: z_0
    real,allocatable,dimension(:):: x, g  
    integer:: i, N
    real:: p_min,p_max,inc,cont_soma
    !==============================================================

    Write(*,*)'Entre com o valor de densidade da esfera em g/cm^3:'
    Read(*,*) delta_rho
    write(*,*)'Entre com o valor do raio da esfera em m:'
    Read(*,*) R
    Write(*,*)'Entre com o valor da profundidade do centro da esfera em m:'
    Read(*,*) z_0

    n = 100
    allocate(x(n),g(n))

!===============================================================
    p_min = -50.
    p_max = 50.
    inc = abs(p_max - p_min)/N

    !inicio do contador
    cont_soma = p_min
    do i = 1, N
        x(i) = cont_soma
        cont_soma = cont_soma + inc 
    end do 

!=================================================================

!conversao de rho para kg/m^3

    delta_rho = delta_rho*1000.
   
!calculo da gravidade
   do i=1, N 
    g(i) = func_grav(delta_rho,R,z_0,x(i))*1.e+5!transforma m/s para miliGal
   end do     
    

   open(101, file='q_7_1_gravimetria_1_esfera.dat', status='replace',action='write')

   do i=1, n
    write(101,*)x(i),g(i)
   end do
close(101)

write(*,*)'Termino da criacao dos dados'

!================================================================

end program gravidade_1_esfera