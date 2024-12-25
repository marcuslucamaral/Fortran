!================================================================================

    ! Purpose:
    ! Escrever um programa que receba as quantidades de quilômetros e de litros em cada viagem. O pro-
    ! grama então deve calcular e exibir o consumo em quilômetros por litro para cada trecho.
    !Depois de processar toda a informação de entrada o programa deve calcular e exibir os
    !quilômetros por litro combinados de todos os tanques cheios. A saída deve ser algo assim:
    ! Quantos litros? (-1 para terminar)
    !40.5
    !Quantos quilômetros?
    !465.7
    !No trecho 1: 11.4987659 km/l
    !Quantos litros? (-1 para terminar)
    !38.7
    !Quantos quilômetros?
    !425.
    !No trecho 2: 10.9819117 km/l
    !Quantos litros? (-1 para terminar)
    !30.
    !Quantos quilômetros?
    !360.
    !No trecho 3: 12.0000000 km/l
    !Quantos litros? (-1 para terminar)
    !-1
    !Em todo o percurso: 11.4532967 km/l
    !
    !
    ! Record of revisions:
    ! Date_begin/end_date             Programmer                             Description of change
! ========                     =============================                      =============
    ! 14/04/23                 Marcus Lucas Amaral do Amaral                      Original code
    ! 14/04/23
!================================================================================

program consum
implicit none
real:: l,k,sum_l,sum_k,med_kml,med_kml_total
integer::i

write(*,*)'entre com o vslo em litros e km, para encerrar a soma:l=-1'
i=0
med_kml_total = 0.
sum_l=0.
sum_k=0.

do
    i=i+1
    read(*,*) l
    if(l==-1) exit
    read(*,*) k
    med_kml=(k/l)
    write(*,*)'no trecho',i,':',med_kml,'km/l'
    sum_l=sum_l+l
    sum_k=sum_k +k 

end do

med_kml_total= sum_k/sum_l

write(*,*)'em todo percurso:',med_kml_total,'km/l'


end program consum
