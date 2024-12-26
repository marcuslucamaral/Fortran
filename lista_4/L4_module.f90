Module lista04
    Implicit None

    Real, parameter:: pi = 3.1415926535847

    Contains

    subroutine create_solution (M,N,A,x,b)
        !Para gerar uma solução adequada, considerando que a matriz de coeficientes (A) é aleatória e o vetor x representa nossa função de teste.
        implicit none
        
        integer,intent(in):: M, N
        real,intent(in):: A(M,N), x(N)
        real,intent(out):: b(M) 
        integer:: i
        
        do i = 1, M
            b(i) = dot_product(A(i,:),x)
        end do

    end subroutine create_solution
    
    Subroutine Elimina(N,A,b)
        implicit none
        integer, intent(in):: N
        real, intent(inout):: A(N,N), b(N)
        real::temp(N)
        integer:: ip
        integer:: i,j,k
        real:: m, pivo, btemp

        Do k = 1, N-1
            pivo = abs(A(k,k))
            ip = k
            do i = k+1, N	
                if (A(i,k) > pivo) then
                    pivo = A(i,k)
                    ip = i
                End if
            end Do
            temp = A(k,:)
            A(k,:) = A(ip,:)
            A(ip,:) = temp	
            btemp = b(k)
            b(k) = b(ip)
            b(ip) = btemp	
            
            Do i = k+1, N
                m = A(i,k) / A(k,k)
                Do j = k+1,N
                    A(i,j) = A(i,j) - m*A(k,j)
                end Do
                b(i) = b(i) - m*(b(k))
            end do
        end do

    End Subroutine Elimina
    
    Subroutine LU_pivoteamento(N,A,b)
    
        implicit none
        integer, intent(in):: N
        real, intent(inout):: A(N,N), b(N)
        real::temp(N)
        integer:: ip
        integer:: i,j,k
        real:: m, pivo, btemp

       !Pivoteamento
       Do k = 1, N-1
           pivo = abs(A(k,k))
           ip = k
           do i = k+1, N	
               if (A(i,k) > pivo) then
                   pivo = A(i,k)
                   ip = i
               End if
           end Do
           temp = A(k,:)
           A(k,:) = A(ip,:)
           A(ip,:) = temp	
           btemp = b(k)
           b(k) = b(ip)
           b(ip) = btemp
            
       !LU
		Do k = 1, N-1
			Do i = k+1, N
 			m = A(i,k) / A(k,k)
				Do j = k+1,N
				A(i,j) = A(i,j) - m*A(k,j)
				end do
				A(i,k) = m
			end do
		end do
            
	End Subroutine LU_pivoteamento(N,A,b)

End Module lista04
