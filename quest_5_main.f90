program conv_main_grau
implicit none
integer :: valor_conv, minut
real :: rad, segund

write(*,*) 'Valor em radianos?'
read(*,*) rad
call conver_grau(rad, minut, segund, valor_conv)

write(*,*) 'Resultado:', valor_conv, 'graus', minut, 'min', segund, 'seg.'
end program conv_main_grau
