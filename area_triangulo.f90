real function area (a1,a2,a3,b1)


        implicit none
        real, intent(in):: a1,a2,a3,b1
    
            !b1 = (a1+a2+a3)/2
            area = sqrt(b1*(b1-a1)*(b1-a2)*(b1-a3))
            

    end function area

program area_triangulo
        !!programa que utiliza uma funcao para calcular 
        !! a distancia entre dois pontos
    
    implicit none 
    
    real:: area,a,b,c,p,S2
    
        write(*,*) 'Quais os valores do lados do triangulo: a, b, c ?'
        read(*,*) a, b, c
        p = (a+b+c)/2. 
        s2 = area(a, b, c, p)

        write(*,*) 'A area é ', S2, 'metros quadrados'

end program