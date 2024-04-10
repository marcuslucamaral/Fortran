real function distancia (x1,x2,y1,y2)


        implicit none
        real, intent(in):: x1,x2,y1,y2
    

            distancia = sqrt((x2-x1)**2+(y2-y1)**2)
            

    end function distancia

program distancia_entre_dois_pontos
        !!programa que utiliza uma funcao para calcular 
        !! a distancia entre dois pontos
    
    implicit none 
    
    real:: distancia,a1,a2,b1,b2,d
    
        write(*,*) 'Quais sao os valores dos pontos: a1, a2, b1, b2 ?'
        read(*,*) a1, a2, b1, b2

        d = distancia(a1, a2, b1, b2)

        write(*,*) 'A distancia é ', d

end program