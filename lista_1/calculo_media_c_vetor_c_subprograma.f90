!================================================================================

    ! Purpose:
    ! Subrotina com Cálculo Média e desvio padrao de um vetor
    !
    ! Record of revisions:
    ! Date_begin/end_date             Programmer                             Description of change
    ! ========                     =============================                      =============
    !april/23                 Marcus Lucas Amaral do Amaral                      Original code
    !
!================================================================================


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

program calcula_media
    implicit none
    integer:: i 
    integer:: N
    integer, parameter:: Nmax = 10000 !reservado de memoria para o vetor
    real::x(Nmax)
    real:: dp, media
    
    open(101, file='dados.dat',status='old',action='read')
    
    read(101,*) N
    

    do i = 1,N
        read(101,*) x(i)
    end do

    close(101)

call Media_dp(N,x, media, dp)!esse  ne nao pode ser menor que o n_max

    
    
    
    write(*,*)'media',media
    write(*,*)'dp',dp


    end program calcula_media
