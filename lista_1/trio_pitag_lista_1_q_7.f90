!===========================================
!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!===========================================
!
! funçao verificaçao se é triangulo valido
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