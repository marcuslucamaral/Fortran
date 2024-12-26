!================================================================================
 
    ! Purpose:
    !
    ! Se conhecemos o logaritmo de um número N na base b, podemos calcular o seu logaritmo
    !na base a através da fórmula.
    !log_a (x) = log_b (x)/log_b (a) 
    !Escreva uma função com dois argumentos de entrada que calcule o logaritmo de um número
    !x em uma base a, qualquer.
    !
    ! Record of revisions:
    ! Date_begin/end_date                 Programmer                             Description of change
    ! ========                     =============================                      =============
    ! unknown                            Marcus Amaral                               Original code
    ! unknown
!================================================================================

real function log_base_a (a,x)


implicit none
real, intent(in):: a,x

    
if (x < -a) then    
    chapeu = 0.

else if (x < 0.) then
    chapeu = (a+x)/a

else if (x < a) then
    chapeu = (a-x)/a
else
    chapeu = 0
   
end if

end function chapeu
