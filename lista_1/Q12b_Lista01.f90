!===========================================
!Programador: Marcus Lucas Amaral do Amaral
!Número de matrícula:202301670006 
!Data:13/04/2023
!===========================================
!================

Integer Function Dice()
Implicit None

Real::na
Integer::num_dado

Call Random_number(na)

!na = na*0.9999999    !Isso era para garantir que o limite superior não seria incluído (ou seja, sempre vai ser menor que 1). Mas, pela definição...
			          !... a função RANDOM_NUMBER é não inclusiva no limite superior. Então, isso não é necessário.
			          
num_dado = int(6*na) + 1

End Function

!############################################################

Logical Function CRAPS()
Implicit None

Integer::Dice, lance_um, lance_dois, total

Write(*,*)'------------------------CRAPS------------------------' 
Write(*,*)'-----------------------------------------------------'
Write(*,*)'Lance o primeiro dado (Pressione ENTER).'
Write(*,*)'Para sair, digite Ctrl+c.'
Write(*,*)'-----------------------------------------------------'
Read(*,*)
lance_um = Dice()
Write(*,*) lance_um
Write(*,*)'	'

Write(*,*)'-----------------------------------------------------'
Write(*,*)'Lance o segundo dado (Pressione ENTER)'
Write(*,*)'-----------------------------------------------------'
Read(*,*)
lance_dois = Dice()
Write(*,*) lance_dois
Write(*,*)'	'

total = lance_um + lance_dois

If (total == 7 .OR. total == 11) then
	CRAPS = .true.
	Write(*,*)'/////////////////'
	Write(*,*)'O JOGADOR GANHOU!'
	Write(*,*)'\\\\\\\\\\\\\\\\\'

else if(total == 2 .OR. total == 3 .OR. total == 12) then
	CRAPS = .false.
	Write(*,*)'|||||||||||||||||'
	Write(*,*)'O JOGADOR PERDEU!'
	Write(*,*)'|||||||||||||||||'

else
	Write(*,*)'                                 --------'
	Write(*,*)'Seu ponto agora é igual à ', total
	Write(*,*)'                                 --------'
	Write(*,*)'Tente repetir seu ponto com o lançamento dos dados.'
	Write(*,*)'Mas cuidado! Se a soma der 07, você perde.'
	DO
		Write(*,*)'-----------------------------------------------------'
		Write(*,*)'Lance o primeiro dado (Pressione ENTER).'
		Write(*,*)'-----------------------------------------------------'
		Read(*,*)
		lance_um = Dice()
		Write(*,*) lance_um
		Write(*,*)'	'
		
		Write(*,*)'-----------------------------------------------------'
		Write(*,*)'Lance o segundo dado (Pressione ENTER)'
		Write(*,*)'-----------------------------------------------------'
		Read(*,*)
		lance_dois = Dice()
		Write(*,*) lance_dois
		Write(*,*)'	'
		
		If ((lance_um + lance_dois) == total) then
			CRAPS = .true.
			Write(*,*)'/////////////////'
			Write(*,*)'O JOGADOR GANHOU!'
			Write(*,*)'\\\\\\\\\\\\\\\\\'
			exit
		else if((lance_um + lance_dois) == 7) then
			CRAPS = .false.
			Write(*,*)'|||||||||||||||||'
			Write(*,*)'O JOGADOR PERDEU!'
			Write(*,*)'|||||||||||||||||'
			exit
		else
		Write(*,*)'-----------------------------------------------------'
		Write(*,*)'Outra chance...'
		Write(*,*)'-----------------------------------------------------'
		End if
	End Do
End If	
		
End Function CRAPS

!##################################################################

Program Aposta
Implicit None

Logical:: CRAPS, jogar
Real:: saldo, valor

saldo = 1000

Write(*,*)'------------------------------------------------------------'
Write(*,*)'               Vamos apostar jogando CRAPS?!                '
Write(*,*)'------------------------------------------------------------'
Write(*,*)'Você tem R$1000. Quanto quer apostar na primeira rodada?'
Write(*,*)''
Read(*,*) valor

Do
	IF (saldo > 0) then
		If (valor <= saldo) then
			jogar = CRAPS()
			if (jogar .eqv. .true.) then
				saldo = saldo + valor
				Write(*,*)'Você ganhou a aposta. Seu saldo é: ', saldo
			else
				saldo = saldo - valor
				If (saldo <= 0) then 
					Write(*,*)'Você Faliu! =/'
					stop
				else
					Write(*,*)'Você perdeu essa. Seu saldo é: ', saldo
				end if
			end if
		Else
			Write(*,*)'Saldo insuficiente.'
		End If
		Write(*,*)'Digite o valor da nova aposta:'
		Read(*,*) valor
	ELSE

	END IF
	
END DO

End Program

