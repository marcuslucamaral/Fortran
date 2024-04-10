  !=============================================================================================
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
    real, parameter :: q = 1.6e-19 !q=1.
    real, parameter :: e_zero = 8.854e-12 !permissividade eletrica no vácuo

    real::a ! distancia carga centro 
    real:: h !altura entre linha de medida e da carga
    real:: k !constante
    real:: p!momento de dipolo
    real,allocatable,dimension(:):: x, Ub, Ud  !valores da coordenada
    integer:: i, N
    real:: p_min,p_max,inc, cont_soma
    !==============================================================
    h = 1.
    a= 0.5
    k = 1./(4.*pi*e_zero)
    !k = 1.
    p = -2.*a*q!momento de dipolo
    N = 1000
    
    !Alocacao dos vetores
    allocate(x(N+1),Ud(N+1),Ub(N+1))

    !calculo de valores de amostras
    p_min = -30
    p_max = 30
    inc = abs(p_max - p_min)/N 

    !inicio da contagem
    cont_soma = p_min
    do i=1, N+1
        x(i) = cont_soma
        cont_soma = cont_soma + inc
    end do


    
    !Calculo do Potencial dipolo,Potencial bipolo
    
    do i=1, n+1
     
      Ud(i) = p*k*x(i)/sqrt((x(i)**2)+(h**2))**3
      Ub(i) = k*q*((1./sqrt((x(i)+a)**2+(h**2))-1./sqrt((x(i)-a)**2+(h**2))))
    end do
    
    ! Salvando em um dado
    open(101, file='q_6_plot_h_1.dat', status='replace',action='write')
    
    do i = 1, n+1
    write(101,*)x(i),Ud(i),Ub(i)
    end do
    
    close(101)
    write(*,*)'Termino da criacao dos dados'
    end program potencial_di_bi