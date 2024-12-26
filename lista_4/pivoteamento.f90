
subroutine Elimina(N,A,b)
    implicit none
    integer,intent(in):: N 
    real,intent(out):: A(N,N),b(N)
    real::temp(N)
    Real::pivo,btemp 
    integer::iP !indice da kinha escolhida para ter pivo
    integer:: i,j,k 
    real:: m 

do k=1,N-1
    pivo=abs(A(k,K))
    IP = k
    do i =k+1,N 
        if (abs(A(i,k))>pivo)then
        pivo = A(i,k)
        Ip=i
        end if
    end do

    temp = A(k,:)
    A(k,:) = A (Ip,:)
    A(Ip,:) = temp
    btemp = b(k)
    b(k) = b(Ip)
    b(Ip) = btemp

    do i = k+1,N 
        m = A(i,k)/A(k,k)
        do j = k+1,N 
            A(i,j) = A(i,j) - m*A(k,j)
        end do
        b(i) = b(i) - m*b(k) 
    end do
end do    

end subroutine Elimina 