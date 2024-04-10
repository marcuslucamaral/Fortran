        !funcao que calcula os valores para a funcao chapeu dado seus limites e o valor de x
real function chapeu (a,x)


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

program funciones_chapel
        !!programa que utiliza valores da funçao chapeu 
        !! 
    
    implicit none 
    
    real:: chapeu, b, y , Valor_final 
    
        write(*,*) 'Quais os valores do intervalo b e do valor y ?'
        read(*,*) b, y
         
        valor_final = chapeu(b,y)

        write(*,*) 'O valor da funcao chapeu é ', valor_final, 'cm'

end program funciones_chapel