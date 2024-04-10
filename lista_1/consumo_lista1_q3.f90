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