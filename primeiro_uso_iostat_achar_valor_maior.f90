program encontra_maior
    implicit none
    
    integer:: I_maior, i, erro
    real:: x, maior
    
    
    
    open(101, file='data.dat',status='old',action='read') ! cuidado ao por o status, se por replace ele ja cria um novo arquivo em cima
    read(101,*,iostat=erro)x
    if(erro == 0 ) then 
        maior = x ! o primeiro numero da lista nao se compara com ninguem por isso poe fora do loop
        i=1
        I_maior = i!se a primeira leitura é x, entao o indice é 1
        do 
            read(101,*,iostat=erro)x
            if(erro==0)then
                i=i+1
                if (x>maior) then
                    maior = x
                    I_maior=i
                end if    
            else
                exit
            end if
        end do    

        write(*,*)'Maior valor', maior, 'na posicao',I_maior, 'de', i
    else
        write(*,*) 'Arquivo vazio' 
    end if
    
    end program encontra_maior