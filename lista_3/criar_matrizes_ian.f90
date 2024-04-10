PROGRAM construir_imagem
  
    IMPLICIT NONE
    
    INTEGER :: Nx, Ny, i, j, k
    REAL, ALLOCATABLE :: red(:,:), green(:,:), blue(:,:)
    REAL, ALLOCATABLE :: data(:)

    ! Leitura dos dados do arquivo
    OPEN(UNIT = 1, FILE = 'fontes.dat', STATUS = 'old', ACTION = 'read')
    READ(1,*) Nx, Ny

    ! Alocação das matrizes de cores
    ALLOCATE(red(Ny, Nx), green(Ny, Nx), blue(Ny, Nx))
    ALLOCATE(data(3*Ny*Nx))

    ! Leitura dos dados das cores
    DO k = 1, 3*Ny*Nx
        READ(1, *) data(k)
    END DO
    CLOSE(1)

    ! Organização dos dados em matrizes de cores
    DO j = 1, Nx
        DO i = 1, Ny
            red(i, j) = data(i + (j-1)*Ny)
            green(i, j) = data(Nx*Ny + i + (j-1)*Ny)
            blue(i, j) = data(2*Nx*Ny + i + (j-1)*Ny)
        END DO
    END DO

    OPEN(1, FILE = 'Red.dat', STATUS = 'replace', ACTION = 'write')
        DO i = 1, Ny
            WRITE(1,*) red(i, :)
        END DO
    CLOSE(1)

    OPEN(2, FILE = 'Green.dat', STATUS = 'replace', ACTION = 'write')
        DO i = 1, Ny
            WRITE(2,*) green(i, :)
        END DO
    CLOSE(2)

    OPEN(3, FILE = 'Blue.dat', STATUS = 'replace', ACTION = 'write')
        DO i = 1, Ny
            WRITE(3,*) blue(i, :)
        END DO
    CLOSE(3)

    DEALLOCATE(red, green, blue, data)
    
END PROGRAM construir_imagem