program ordena_vetores
    !
    ! Purpose:
    ! O arquivo coordenadas.dat contém três colunas contendo as coordenadas (x, y) de 500 pontos
    !do gráfico de uma função, no seguinte padrão:
    ! 500
    !1 5.9853573 -2.9344454
    !2 3.4009480 -2.5645757
    !3 5.7954922 -4.6858921
    !.     .         .
    !Os valores na primeira coluna são índices inteiros, na segunda e na terceira estão as coordenadas
    !x e y, respectivamente. Acontece que os pontos não estão dispostos sequencialmente no arquivo.
    !Eles foram embaralhados de forma aleatória.
    !Escreva um programa para realizar as seguintes tarefas:
    !1. Ler os dados do arquivo coordenadas.dat;
    !2. Criar um segundo arquivo contendo as mesmas coordenadas, agora ordenadas de acordo
    !com os valores da coluna dos x.
    !3. Construa o gráfico da função y = f(x).
    !
    !
    ! Record of revisions:
    ! Date Programmer Description of change
    ! ======== ============================= =============
    ! 16/05/23 Marcus Lucas Amaral do Amaral Original code

!================================================================================

    implicit none
    integer :: N, i
    

    real, allocatable:: x(:),y(:) 
    integer, allocatable:: ind(:)

    open(101, file='coordenadas.dat',status='old',action='read')
    open(102, file='coordenadas_ordenadas.dat',status='replace',action='write')
    read(101,*) N

    allocate(ind(N),x(N),y(N))


    do i = 1,N
        read(101,*) ind(i),x(i),y(i)
    end do

    call bolha_alg(n,x,y)

    !write(102,*)N
    
    do i = 1, N
    write(102,*) ind(i),x(i),y(i)
    end  do

    close(101)
    close(102)

    contains


    subroutine bolha_alg(n, x, y)
        implicit none
        integer, intent(in) :: n
        real, intent(inout) :: x(n),y(n)
        integer :: i, j 
        real :: aux,aux_y
        integer :: verif
        do j = 1, n-1
            verif = 0
            do i = 1, n-j
                if (x(i) > (x(i+1))) then
           !ordenando valores de x
                    verif = verif + 1
                    aux = x(i)
                    x(i) = x(i+1)
                    x(i+1) = aux

            ! ordenando valores de y
                   aux_y = y(i) 
                   y(i) = y(i+1)
                   y(i+1) = aux_y
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

    
    

end program ordena_vetores
