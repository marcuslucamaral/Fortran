!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!================

Program Dice
Implicit None

Real::na
Integer::num_dado

	write(*,*)'-----------------------------------------------------'
	Write(*,*)'Vamos jogar? Para lançar o dado, pressione ENTER.' 
	Write(*,*)'Ctrl+c, para sair.'
	write(*,*)'-----------------------------------------------------'
	
Do

	Read(*,*) 
	
	Call Random_number(na)

!	na = na*0.9999999 !para garantir que o limite superior não é incluído (ou seja, sempre vai ser menor que 1). Mas, pela definição...
			          !... a função RANDOM_NUMBER é não inclusiva no limite superior. Então, isso não é necessário.

	num_dado = int(6*na) + 1

	write(*,*)'-----------------------------------------------------'
	write(*,*)'O resultado desse lançamento foi:',num_dado
	write(*,*)'-----------------------------------------------------'

end Do	

End Program
