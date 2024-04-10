Module estatistica
    implicit none
    
    contains
    
    subroutine Media_dp(N,x, media,dp)
        implicit none
        integer,intent(in):: N
        real,intent(in)::x(n)
        real,intent(out)::media,dp
        real:: soma, soma_2
        integer:: i
        
        soma = 0.
        soma_2 = 0. 
        
        do i =1,N
         
            soma = soma + x(i)
            soma_2=soma_2+x(i)**2
        end do
        
        media=soma/n
        dp=sqrt(soma_2/n - media**2)
        end subroutine Media_dp

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
        subroutine bolha_alg(n, x)
            implicit none
            integer, intent(in) :: n
            real, intent(inout) :: x(n)
            integer :: i, j 
            real :: aux
            integer :: verif
            do j = 1, n-1
                verif = 0
                do i = 1, n-j
                    if (x(i) > (x(i+1))) then
                        verif = verif + 1
                        aux = x(i)
                        x(i) = x(i+1)
                        x(i+1) = aux
                    end if
                end do
                if (verif == 0) then
                exit
                else 
                
                end if
                !Para verificar quantas voltas 
                !write(*,*) 'número de voltas:', j
                !!do i = 1, n
                 !!   write(*,*) x(i)
                !!end do 
            end do
            write(*,*) 'número de voltas:', j - 1
            
            end subroutine bolha_alg
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


            
    end module estatistica