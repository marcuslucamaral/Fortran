subroutine quadratica( A, B, C, Nr, R1, R2)
    
    !
    !routine resolve eq. segundo grau : Ax^2 + Bx +C=0
    !

    implicit none
    real, intent (in) :: A, B, C
    real, intent (out) :: R1, R2
    Integer, intent (out) :: Nr
    Real:: Delta

    Delta = B** 2 - 4*A*C
        if (Delta< 0.) then 
            Nr = 0
        else if (Delta > 0.) then
            Nr = 2
            R1 = (-B - sqrt(Delta))/ 2*A
            R2 = (-B + sqrt(Delta))/ 2*A
        else
            Nr = 1
            R1 = B/ (2*A)
            R2 = R1
        end if 

    end subroutine quadratica


        Program teste_quadratica

            implicit none
            Real:: C1, C2, C3
            Real:: Raiz_1, Raiz_2
            Integer:: N

            write(*,*)'Solução da equacao Ax^2 + Bx +C=0.'
            write(*,*)'Escreva os coeficientes da equacao A + B +C:'
            Read(*,*) C1, C2, C3

            call quadratica (C1, C2, C3, N, Raiz_1, Raiz_2)
            
            if (N == 1) then
                Write(*,*)'Uma raiz:', Raiz_1
            else if (N == 2) then
                
                    Write(*,*)'Duas Raizes:'
                    Write(*,*)'R1:', Raiz_1
                    Write(*,*)'R2:', Raiz_2
                else
                    Write(*,*)'nao ha raiz real'

                end if

            end program teste_quadratica