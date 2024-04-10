real function gz (r,rho,x,y,z)
    real, parameter:: pi = 3.14159265359
    real, parameter:: G = 6.674e-11
    real, intent(in):: r,rho,x,y,z ! r é o raio da esfera, rho é o contraste da densidade (entre a esfera e o campo envolta), x é a distancia no plano x e z é a profundidade (altura) que a esfera está
    !real:: gz

    gz = ((G*(4./3.)*pi*r**3*rho*z)/((x**2 + y**2 + z**2)**1.5))*10**5

end function gz


program esferas
    implicit none 
    real:: gz
    real,allocatable,dimension(:):: r, rho, z, x, x0, y, y0
    real, allocatable:: gg(:,:), distx(:),disty(:)
    integer::e, i, j, k, n ! e é a quantidade de esferas, i, j e k são os contadores, n é o total de aquisições
    real,allocatable:: f(:,:), xy(:,:)


    write(*,*)'Quantas esferas'
    read(*,*)e
    write(*,*)'Quantidade de aquisicoes'
    read(*,*)N

    Allocate(gg(N,N),r(e),rho (e), x(N),x0(e), y(N), y0(e), z(e), distx(e), disty(e))
    Allocate(xy(N,2),f(N,N))

    !! Para as 3 esferas do exemplo da questão 7.2. Com o y0 na origem.
    !esfera 1
    r(1) = 10.
    rho(1) = 2.*1.e3
    z(1) = 11.
    x0(1) = -15
    y0(1) = 0.

    !esfera 2
    r(2) = 10.
    rho(2) = 2.*1.e3
    z(2) = 25.
    x0(2) = 0.
    y0(2) = 0.

    !esfera 3
    r(3) = 10.
    rho(3) = 4.*1.e3
    z(3) = 11.
    x0(3) = 20
    y0(3) = 0.

    !! PARA N ESFERAS
    ! do i = 1,e
    !     Write(*,*) 'Qual o raio da esfera',i
    !     read(*,*)r(i)

    !     Write(*,*) 'a densidade (g/cm^3) da esfera',i
    !     read(*,*)rho(i)
    !     rho(i) = rho(i)*2.e3

    !     Write(*,*) 'a profundidade (m) da esfera',i
    !     read(*,*)z(i)

    !     Write(*,*) 'a posicao x (m) da esfera',i
    !     read(*,*)x0(i)

    !     Write(*,*) 'a posicao x (m) da esfera',i
    !     read(*,*)y0(i)


    ! end do


    do i = 1,e-1
        distx(i)=((x(i)- x(i+1))**2 + (z(i) - z(i+1))**2 )
        disty(i)=((y(i)- y(i+1))**2 + (z(i) - z(i+1))**2 )
    end do

    do i = 1,e-1
        if ((distx(i) >= r(i)+r(i+1)) .and. (disty(i) >= r(i)+r(i+1))) then
            write(*,*) 'Esfera',i,'Nao sobrepoe a Esfera',i+1  
        else 
            write(*,*) 'Esfera',i,'sobrepoe a Esfera',i+1  
            exit
        end if 
    end do

    f(:,:) = 0.
    do j = 1,e ! quantidade de esferas
        do k = 1,N
            y(k) = k - (N/2) - y0(j) !variando em y
            do i= 1,N
                x(i) = i - (N/2) - x0(j) ! variando em x
        
                gg(i,k) = gz(r(j),rho(j),x(i),y(k),z(j)) ! vai ser uma matriz pra cada esfera, pra só depois somar todas ponto a ponto
            end do
        end do
        f = gg + f ! armazenando a soma das matrizes
    end do

    xy(:,1) = x(:)
    xy(:,2) = y(:)
    xy = transpose(xy)


    open(98, file = 'xy.dat', status = 'replace', action = 'write')
    write(98,101)xy
    101 format (2F7.2,2x)

    open(99, file = 'esferas.dat', status = 'replace', action = 'write')
    write(99,102)f
    102 format (100ES28.17,x)


end program esferas

