Program distancia   
    ! Programa para calcular a distancia entre dois pontos
    ! Programador
    ! ...
    Implicit none
    Real :: x1, x2, y1, y2   !x_n=coordenadas no eixo x em metros;y_n=coordenada no eixo y metros
    Real :: d            !d = distancia entre dois pontos em metroa    

    Write(*,*) 'Qual a distancia entre dois pontos? Digite os valores x1, x2, y1, y2'
    Read (*,*) x1 , x2, y1, y2

    d = sqrt((x2-x1)**2+(y2-y1)**2) 

    Write(*,*) "distancia entre dois pontos", d, 'm'

End program
