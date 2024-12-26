!================================================================================
 
    ! Purpose:
    !
    ! Qualquer conjunto de três números inteiros que são lados de um triângulo retângulo é
    !chamado de Trio Pitagórico. Encontrar todos os trios pitagóricos formados por inteiros não
    !maiores do que 500. Use laços de DO para testar todas as possibilidades. Construa os
    !laços de maneira a não gerar triângulos repetidos. Este é um exemplo de computação de
    !“força bruta”. Em cursos mais avançados você vai aprender que existem vários problemas
    !interessantes para os quais não há nenhuma abordagem algorítmica conhecida a não ser
    !usar pura força bruta.
    !
    !
    ! Record of revisions:
    ! Date_begin/end_date                 Programmer                             Description of change
    ! ========                     =============================                      =============
    ! 14/04/23                            Marcus Amaral                               Original code
    ! unknown
!================================================================================
Program TrioPitagorico 
    Implicit None
    
    Integer::a, b, c, n_vol
    Real:: soma_cat
    
    n_vol = 1
    
    Do a = 1, 500
        Do b = a+1, 500
            Do c = b+1, 500
    
                soma_cat = (a**2) + (b**2)
                
                If (c <= 500 .and. c**2 == soma_cat) then
                    Write(*,*)'Trio Pitagórico',n_vol,':',a,b,c
                    n_vol = n_vol + 1
                else
                end if
                         
            End Do
        End Do
    End Do
    
End Program
