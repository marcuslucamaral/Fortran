!procurar um pacote chamado BLAS varias subroutines de resoluçao
!IMSL outra mt boa mas pago
!computador intel tem uma biblioteca chamada MKL

subroutine Fatora_Lu(N,A)
implicit none 
integer,intent(in)::N 
real(8), intent(inout):: A(N,N)
integer:: i,j,k 
real(8)::m 

do k = 1, N - 1
	do i = k + 1, n
		m = A(i,k) / A(k,k)
			do j = k+1, N
				A(i,j) = A(i,j) - m * A(k,j)
			end do
            !A(i,k+1:N) = A(i,k+1:N) - m*A(k,k+1:N)
            A(i,k) = m 

	end do
end do

end subroutine Fatora_LU

subroutine resolve(N,A,b)
    !Recebe a matriz na forma LU e executa as etapas
    !de substituição rdireta e reversa
    !retorna a solução do vetor b
implicit none
integer,intent(in):: N
real(8),intent(in):: A(N,N)
real(8), intent(inout)::b(N)
integer::i,j
real(8)::soma

!substiruição direta
do i=2,n
    soma=0.
    do j = 1,i-1
        soma = soma +A(i,j)*b(j)
    end do 
    b(i)=b(i)-soma 
end do 
 
! Retrosubstituição 
b(N) = b(N)/A(N,N)

do i= N-1,1,-1
    soma = 0.
    do j=i+1,N
        soma = soma + A(i,j)*b(j)
    end do
    b(i) = (b(i)-soma)/A(i,i)
end do


end subroutine resolve




!call Prod_Mat_Vet(N,N,A,f,b)

!call Fatora_LU(N,A)
!call Resolve(N,A,b)

!open(101,file='Solucao.dat',status='replace')

!    do i=1,N
!        write(101,*) t(i),f(i),b(i)
!    end do