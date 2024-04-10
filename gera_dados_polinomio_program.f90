program poli_random
        implicit none
        Real(8), allocatable :: x(:),y(:),c(:)!,r(:)!c=vetor de coeficientes, r=vetro de ruido
        Integer:: g, N!g = grau do polinômio, N = numero de pontos
        Real(8):: w, x_i,x_f! w = amplitude do ruído, x_i e x_f é o intervalo de x
        Real(8)::r 
        Real(8):: sup_sum 
        Real(8):: dist_x 
        real(8):: coef_aux
        real(8):: coef 
        Integer:: i,j
!====================================================================



!Leitura dados de entrada
        open(101, file = 'dados_poli.dat', status = 'old', action = 'read')
        open(102, file = 'dados_poli_output.dat', status = 'replace', action = 'write')
        !esta na seguinte ordem
        !g
        !c0. c1. c2. ... cn.
        !w
        !x_i  x_f  N
        read(101,*)g
        allocate(c(0:g))!alocação de coeficientes
        !vetor c vai de 0 ate cn exemplo: y =c(0)+c(1)x+c(2)x**2 +...
        read(101,*)c
        read(101,*)w
        read(101,*)x_i,x_f,N
      
        !alocaçao de vetores
        allocate(x(N),y(N))

!=======================================================================        
        !intervalo de x
        dist_x = abs(x_f - x_i)/N
        
        sup_sum = x_i 
        do i = 1, N 
            x(i) = sup_sum 
            sup_sum = sup_sum + dist_x
        end do

!====================================================================== 

        !calculo de y 
        y = 0.
        do i = 1, N
            
            do j = 0, g
                y(i) = y(i) + c(j) * x(i)**j
            end do
        end do

!=========================================================
!arquivo de saída
            
        do i=1,N
            call random_number(r) ! ele gera numeros entre fechado no 0 e aberto no 1
                r = (r-0.5)*2*w
            write(102,*)x(i),y(i)+r, y(i)        
        end do

!close(101)
!close(102)
!===============================================================
write(*,*)'Termino da geracao de dados'
!===============================================================

end program poli_random