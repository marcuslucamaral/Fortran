integer function lanca_dado()
implicit none
real:: x
    call random_number(x)
    lanca_dado = int(x*6 +1) 

end function lanca_dado
!!!!!!!!!!!!!!!!
program faces_dado

    implicit none
    real:: x
    integer:: i, F
    integer:: lanca_dado
    do i = 1,10
        F = lanca_dado()
            write(*,*) i, F
    end do


end program faces_dado