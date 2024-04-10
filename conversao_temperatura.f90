Program Fahrenheit_Celsius
    ! Programa para calcular a conversão de unidades
    ! de temperatura
    ! Programador
    ! ...
    Implicit none
    Real :: F   !F: temperatura em Fahrenheit
    Real :: C   !C: temperatura em Celsius

    Write(*,*) 'Qual a temperatura em Fahrenheit?'
    Read (*,*) F

    C = 5.*(F-32.)/9

    Write(*,*) "Temperatura em Celsius", C

End program
