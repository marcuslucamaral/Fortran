Program gravidade
    ! Programa para calcular a aceleracao da gravidade
    ! Programador
    ! ...
    Implicit none
    Real :: m,r   !m=massa do planeta em kg;r=raio do praneta em m
    Real,parameter :: G = 6.67e-11 !G = constante gravitacional
    Real :: a

    Write(*,*) 'Qual a aceleração da gravidade? Digite a massa e o raio do planeta? '
    Read (*,*) m , r

    a = (G*m)/r**2 

    Write(*,*) "Aceleração da gravidade", a

End program
