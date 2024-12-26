    
program media_movel
 
!============================================================================================
    ! Purpose:
    !Esse programa faz a media móvel de um conjunto de dados
    !
    !
    ! Record of revisions:
    ! Date                         Programmer                         Description of change
    ! ========          =============================                        =============
    ! 18/05/23                     Marcus Amaral                             Original code

!================================================================================
    implicit none
    use estatistica
    integer :: N, i, w
    

    real, allocatable:: x(:),d(:) !entrada de dados
    real,allocatable:: y(:), m(:) !saida de dados
    

    open(101, file='dado_ruido.dat',status='old',action='read')
    open(102, file='dados_ruido_media_movel.dat',status='replace',action='write')
    read(101,*) N

    allocate(x(N),d(N))

    !leitura do dado_ruido
    do i = 1,N
        read(101,*) x(N),d(N)
    end do

    write(*,*)'Entre com o tamanho da janela w'
    read(*,*) w

    call






end program media_movel
