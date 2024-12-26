!============================================================================================
    ! Purpose:
    !Escreva um programa que receba valores de ângulos expressos em radianos e os converta em
    !graus, minutos e segundos. Escreva os resultados em um formato semelhante ao do exemplo:
    !Valor em radianos?
    !1.
    !1.0000000 rad = 57 graus, 17 min e 44.8223877 seg.
    !Repare que os valores em graus e minutos são números inteiros. Apenas os valores em segundos
    !são expressos com variáveis reais.
    !
    !
    ! Record of revisions:
    ! Date                         Programmer                         Description of change
    ! ========          =============================                     =============
    ! may/23                     Marcus Amaral                             Originalcode

!================================================================================

subroutine angulo(rad, graus, minutos, segundos)
        implicit none
        real, intent(in) :: rad
        integer, intent(out) :: graus
        integer, intent(out) :: minutos
        real, intent(out) :: segundos
        real :: conv,nmrvoltas,reducao
        real, parameter :: pi = 3.14159265359

        conv = (rad * 180.) / pi
                
       if (conv < 360.) then
                graus = int(conv)
                minutos = int((conv - graus)*60)
                segundos = (((conv - graus)*60) - minutos)*60
        else
                nmrvoltas = (conv/360.)
                reducao = nmrvoltas - int(nmrvoltas)
                graus = int(reducao*360)
                minutos = int(((reducao*360)-graus)*60)
                segundos = ((((reducao*360)-graus)*60)-minutos)*60
        end if

        end subroutine

        program calculo_angulo
                implicit none
                real :: angrad, angsegundos
                integer :: anggraus, angminutos

                write(*,*)'Entre o valor do ângulo em radianos para conversão:'
                read(*,*)angrad

                call angulo(angrad,anggraus,angminutos,angsegundos)

                100 format (a,es10.3,a)
                200 format (i2,a,i2,a,f6.3,a)

                write(*,100)'O ângulo ', angrad, ' rad, convertido para graus, minutos e segundos fica igual a:'
                write(*,200) anggraus, ' graus ', angminutos, ' minutos ', angsegundos, ' segundos.'

         end program


