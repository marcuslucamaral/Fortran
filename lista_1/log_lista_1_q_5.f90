!===========================================
!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!===========================================
!

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
