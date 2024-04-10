!===========================================
!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!===========================================
!
subroutine conversor(rad, grau, min, sec)
    
    !
    !routine converte angulo de radianos para graus, minutos e segundos
    !

    implicit none

    !declaraçoes de variaveis e constantes
    real, intent (in) :: rad                                !entrada real de angulo em radianos: rad
    integer, intent (out) :: grau, min                      !saida inteira para graus: grau; para minutos:min
    real, intent (out) :: sec                               !saida real para segundos: sec

    Real, parameter:: pi = 3.14159265                       !valor constante de pi: pi
    real:: grau_real, min_real, min_real_linha, sec_real    !variáveis temporárias
        !calculo do grau
        grau_real = rad*(180./pi)                           !calculo do grau em valores reais com entrada de rad
        grau = grau_real                                    !variavel temporaria grau_real que trunca o valor real para inteiro de grau
        !calculo do minuto
        min_real = grau_real - grau                         !calculo da diferença entre o valor real em grau pelo truncado em inteiro para encontrar apenas a parte pós vígula
        min_real_linha = min_real*60.                       !calculo do minuto em valor real, produto do minuto real por 60
        min = min_real_linha                                !variavel temporaria min_real_linha que trunca o valor real para inteiro de min e atribui a variavel min
        !calculo do segundo
        sec_real = min_real_linha - min                     !
        sec = sec_real*60.



    end subroutine conversor


    program teste_conversor
        implicit none
        real:: r1,s1
        integer:: d1, m1

        Write(*,*) 'Valor em radianos?'
        read(*,*) r1


        call conversor(r1, d1, m1, s1)
        Write(*,*) r1,'rad =',d1, 'graus', m1,'min', 'e', s1, 'seg.1'


    end program teste_conversor