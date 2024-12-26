!================================================================================
 
    ! Purpose:
    !
    ! Um dispositivo para lançar projéteis (um canhão, ou morteiro) é capaz de lançar uma
    !bala com velocidade de módulo v0 em um ângulo de lançamento teta. Considere que um
    !lançamento é feito do alto de um penhasco de altura h, como ilustrado na figura.
    !Se ignorarmos a influência do ar e as variações na aceleração da gravidade g com a altura
    !(uma aproximação ruim para velocidades altas) o projétil irá viajar em uma trajetória
    !parabólica e atingir o solo a uma distância horizontal x após um tempo t.
    !Como você pode deduzir facilmente, o tempo para o impacto com o solo é dado pela solução
    !da equação quadrática
    !gt^2 - 2v0 sin(teta)t − 2h0 = 0.
    !Uma vez tendo o tempo, a distância percorrida na horizontal será simplesmente
    !x = v0 cos(teta)t.
    !Escreva um programa que tenha como entrada os dados do modelo e calcule o tempo t e a
    !posição x para o ponto de impacto com o solo, com as seguintes informações:
    !Entradas:                                             Saídas:
    !– Altura h;                                         – Tempo t para o impacto;
    !– Velocidade de lançamento v0;                      – Distância x até o ponto de impacto.
    !– Ângulo de lançamento teta.
    !O programa pode receber os valores diretamente como entradas no teclado e dar a saída
    !no terminal. Escreva uma subroutine ou function para calcular a solução da equação.
    !Você pode usar a rotina que já tem para isto, ou programar uma para este caso particular.
    !Qual você considera a melhor abordagem?
    !
    !Especifique que o ângulo de lançamento é em graus e que não pode ser maior do que 90.
    !O programa deve verificar o valor do ângulo dado como entrada e pedir uma correção caso
    !ele seja maior do que 90◦. Sua rotina irá funcionar para ângulos menores que zero? Se sim,
    !também haverá alguma restrição de ângulo para este caso? Após se certificar das respostas
    !a estas duas perguntas, programe as verificações sobre todas as restrições necessárias e
    !também as inclua em linhas de comentários no seu arquivo do código.
    !
    !Defina a aceleração da gravidade como constante (parameter), com o valor g = 9.8 m/s2.
    !Todos as grandezas físicas devem ser expressas em unidades do Sistema Internacional. Não
    !esqueça de que as funções trigonométricas sin e cos esperam argumentos em radianos.
    !
    ! Record of revisions:
    ! Date_begin/end_date                 Programmer                             Description of change
    ! ========                     =============================                      =============
    ! unknown                            Marcus Amaral                               Original code
    ! unknown
!================================================================================

! The subroutine calculate the solution of the equation with grau two.
subroutine squared( A, B, C, Nr, R1, R2)
    
    ! This subroutine gives the solution of the equation Ax^2+ Bx + C = 0
    
    ! if
    implicit none
    Real, intent(in) :: A, B, C
    Real, intent(out) :: R1, R2
    Integer, intent(out) :: Nr
    real :: Delta
        
    Delta = (B**2) - (4*A*C)
    if (Delta < 0.) then
        Nr = 0
    else if (Delta > 0.) then
        Nr=2
        R1 = (-B -sqrt(Delta)) / (2 * A)
        R2 = (-B + sqrt(Delta)) / (2 * A)
        ! you can save the defoul calculate in a variable to avoit the operation that will problaby need to do the calculate again
    else 
        Nr=1
        R1=-B / (2*A)
        R1=R2
    end if
        
    end subroutine
    
    program tempos_da_escola
    implicit none
    real :: h_o, v_o, rad, degree
    real :: a, b, c
    real, parameter :: g = 9.8
    real, parameter :: pi = 3.1415926535
    real :: t_1, t_2, t, x
    integer :: N
    write(*,*) 'Escreva os valores para: velocidade e altura'
    read(*,*) v_o
    read(*,*) h_o
    
    write(*,*) 'Escreva o valor para o angulo:'
    read(*,*) degree
    
    do 
        if (degree < 0. .or. degree > 90) then
            write(*,*) 'Valor errado tente denovo: escreva um valor menor ou igual a 90 graus' 
            read(*,*) degree
        else
        exit
        end if
    end do
    
    rad = degree * ( pi/ 180. )
    
    a = g
    b = -2 * v_o * sin(rad)
    
    c = -2 * h_o 
    call squared( a, b, c, N, t_1, t_2)
    
        if( N == 1) then
            t = t_1
        else if ( N==2) then
            t = t_2
        else
            write(*,*) 'não tem solução'
            stop
        end if 
    ! Calculo da distância
    
    
    write(*,*) 'O tempo gasto até a posição do impacto:', t
    
    x = v_o * cos(rad) * t
    
    write(*,*) 'A posição de impacto (distância percorrida) x vale:', x
    end program
    
    
    
    
    
    
    
