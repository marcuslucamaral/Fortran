!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!================

Program Test_Taylor
    ! Para testar a nossa implementação para a aproximação de Taylor da função seno.
    
    Implicit None
    Real:: x, xg, valor, unidade
    Real,Parameter::Pi = 3.14159265358979
    real::aux
    
    
    write(*,*) 'Aproximação da função sen(x), utilizando a série de Taylor.'
   
    write(*,*) 'O valor de x está em radianos (digite 1) ou graus (digite 2)?'
    read(*,*) unidade
    
    if (unidade == 1) then
        write(*,*) 'Informe o valor de x, em radianos:'
        read(*,*) x  
    else if (unidade == 2) then
        write(*,*) 'Informe o valor de x, em graus:'
        read(*,*) xg
        x = xg*Pi/180 
    else
       
        Write(*,*) 'Não foi informada uma opção válida. Encerrando a aplicação...'
       
        stop
    end if
        
    if (abs(x) >= 2*Pi) then       !Aqui testamos se o valor informado é maior que 2*Pi.
        aux = x / (2*Pi)           !Se for maior, verificamos quantas vezes maior.
        x = x - int(aux)*2*Pi      !Então subtraímos (se x for positivo) os multiplos de 2*pi e trazemos a aproximação para a primeira volta. 
    else                           !Perceba que se x for negativo, somamos os múltiplos de 2*pi, de modo que a nossa aproximação é sempre... 
    end if                         !...calculada próximo do zero.
        
    valor = seno(x)                !"valor" é a variavél para armazenar a saída da NOSSA função "seno(x)".
    
    
    write(*,*) 'sen(x):', valor, 'Valor da nossa aproximação.'
    write(*,*) '       ', sin(x),'Valor da função intrínseca "sin(x)" do Fortran' !para comparação do valor encontrado pela nossa implementação.
   
    
    Contains
    
    Real Function seno(x)
    Implicit none
    Real, intent(in):: x
    integer:: N
    Real:: t
    
    N=0	
    t=((-1)**N) * x                !Como N = 0, esse valor corresponde ao primeiro termo da série (igual a "x").
    seno = t                       !Perceba que a nossa função é inicializada já com o primeiro termo da série.
    
    Do
        N = N + 1
        t = (-t)*(x/(2*N+1))*(x/(2*N))
        if (seno + t == seno) exit
        seno = seno + t
     
    
    End Do
    
    write(*,*) 'Utilizando', N+1, 'termos na série:'
    
    End Function seno
    
    End Program
    