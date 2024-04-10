program tan_erro
    !
    ! Purpose:
    ! Esse programa calcula tan(theta) em graus, por meio de sen(theta)/cos(theta)
    ! onde a magnitude de cos(theta) => 10^-20 se for menor deve escrever uma 
    ! mensagem de erro
    !
    ! Record of revisions:
    ! Date Programmer Description of change
    ! ======== ============================= =============
    ! 01/05/23 Marcus Lucas Amaral do Amaral Original code

!================================================================================

    implicit none
    real:: theta
    real:: tan

    write(*,*) 'Entre com o valor do angulo em graus'
    read(*,*) theta

    

    if (cosd(theta) < 1.e-20) then
        write(*,*) 'Cosseno do angulo menor que 10^-20 logo tente outro angulo'
     else
        tan = sind(theta)/cosd(theta)
        write(*,*) 'Tangente de theta é igual a',tan
     end if

end program tan_erro