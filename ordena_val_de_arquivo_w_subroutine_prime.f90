subroutine ordena_valores(N,x)
    implicit none
    integer,intent(in):: N
    real,intent(inout)::x(n)
    real:: aux
    integer:: i, j
    logical:: sair 
   do j = 1, N - 1
        sair = .true.

        do i = 1, N - j
            if(x(i)>x(i+1)) then subroutine ordena_valores(N,x)
            implicit none
            integer,intent(in):: N
            real,intent(inout)::x(n)
            real:: aux
            integer:: i, j
            
           do j = 1, N - 1
                do i = 1, N - 1
                    if(x(i)>x(i+1)) then 
                        aux = x(i)
                        x(i) = x(i+1)
                        x(i+1) = aux
                    end if
                end do
            
                write(*,*)'volta', j
                do i=1,N
                write(*,*)x(i)
        
                end do
        end do
        
        
            end subroutine ordena_valores
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            program le_lista_ordena_val
                implicit none
                integer:: i
                integer:: N
                !integer, parameter:: Nmax = 10000 !reservado de memoria para o vetor
                real,allocatable::x(:)
                !real:: dp, media
                
                open(101, file='dados.dat',status='old',action='read')
                
                read(101,*) N
        
                allocate(x(N))
            
                do i = 1,N
                    read(101,*) x(i)
                    write(*,*) x(i)
                end do
            
                call ordena_valores(N,x)
        
                do i = 1,N
                    
                write(*,*) x(i)
                end do
        
                close(101)
            
            
            
                
                
                
               
                
            
            
                end program le_lista_ordena_val
                x(i+1) = aux
                sair = .true.
            end if
        end do
        if(sair) exit
        write(*,*)'volta', j
        do i=1,N
        write(*,*)x(i)

        end do
end do


    end subroutine ordena_valores
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    program le_lista_ordena_val
        implicit none
        integer:: i
        integer:: N
        !integer, parameter:: Nmax = 10000 !reservado de memoria para o vetor
        real,allocatable::x(:)
        !real:: dp, media
        
        open(101, file='dados.dat',status='old',action='read')
        
        read(101,*) N

        allocate(x(N))
    
        do i = 1,N
            read(101,*) x(i)
            write(*,*) x(i)
        end do
    
        call ordena_valores(N,x)

        do i = 1,N
            
        write(*,*) x(i)
        end do

        close(101)
    
    
    
        
        
        
       
        
    
    
        end program le_lista_ordena_val