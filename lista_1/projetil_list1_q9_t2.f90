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
program trajetoria
    implicit none
    real, parameter :: g = 9.8      !Valor da Gravidade 
    real :: h, v0, ang_rad, t, x    !h = altura, v0 = velocidade inicial, ang_rad = variavel que vai receber a função gr2rad, t = tempo até chegar
    integer :: a0


    !Chamada das entradas fornecidas pelo usuário
    write(*,*) 'Entre com os valores da altura em metros'
    read(*,*) h
    write(*,*) 'Agora digite o valor da velocidade de lançamento em m/s'
    read(*,*) v0
    write(*,*) 'Por fim digite o ângulo de lançamento (Atente-se que deve ser entre 0 e 90º)'
    read (*,*) a0

    !Looping para verificar se o ângulo está dentro dos parâmetros
    Do                                    
        if (a0 >= 0 .and. a0 <= 90) exit
        write(*,*) 'O valor não está dentro dos limites, por favor, digite um novo valor'
        read(*,*) a0
    end do      

    !Chama a função que irá converter graus para radianos
    ang_rad = gr2rad(a0)
    write(*,*) 'a conversão para radianos é',ang_rad

    !Chama a rotina que irá calcular o resultado da equação de segundo grau
    call eq2g (g, -(2 * v0 * sin(ang_rad)), -(2*h), t)


    write(*,*) 'O tempo para o impacto do projétil é de',t,'segundos.'

    x = v0 * cos(ang_rad) * t

    write(*,*) 'E a distância até o ponto de impacto é de',x,'metros.'
    
!-------------------------------------------------------------------------------------------------------------!
    CONTAINS
    
        real function gr2rad(a0)
            !Função que irá converter o ângulo em graus fornecido pelo usuário e converter para radianos
            implicit none
            integer :: a0
            real, parameter :: pi = 3.141592

            gr2rad = a0 * (pi / 180)
        end function

        subroutine eq2g (A, B, C, t)
            implicit none
            real, intent(in) :: A
            real, intent(in) :: B, C
            real, intent(out) :: t
            real :: delta, raiz1, raiz2

            delta = (B**2) - 4 * A * C
    
            raiz1 = (-B - sqrt(delta)) / (2 * A)
            raiz2 = (-B + sqrt(delta)) / (2 * A)

            if (raiz1 > raiz2) then
                t = raiz1
            else
                t = raiz2
            end if

        end subroutine

end program
