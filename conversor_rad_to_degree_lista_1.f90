subroutine conversor(rad, grau)
    
    !
    !routine resolve converte angulo em radianos para graus, minutos e segundos
    !

    implicit none
    real, intent (in) :: rad
    real, intent (out) :: grau!, min, sec
    !Integer, intent (out) :: Nr
    Real, parameter:: pi = 3.14159265

    grau =  rad*(180./pi)

    end subroutine conversor


    program teste_conversor
        implicit none
        real:: r1,d1


        Write(*,*) 'Valor em radianos?'
        read(*,*) r1


        call conversor(r1, d1)
        Write(*,*) r1,'rad=',d1, 'graus'


    end program teste_conversor