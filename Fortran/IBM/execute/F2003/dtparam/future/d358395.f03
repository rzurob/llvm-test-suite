
PROGRAM sequentialRead06
    IMPLICIT NONE

    TYPE tN
        CHARACTER(6) :: c
    END TYPE tN

    TYPE tP(l1)
        INTEGER, LEN :: l1
        CHARACTER(l1) :: c
    END TYPE tP

    INTEGER :: i
    CHARACTER(6) :: str( 2 ) = [ 'Hello!', 'to the' ]

    TYPE(tP(:)), ALLOCATABLE :: tIn( : )

    OPEN(41, ACCESS='sequential', ACTION='write', FORM='unformatted')
    WRITE( 41 ) tN(str( 1 ))
    WRITE( 41 ) tN(str( 2 ))
    CLOSE( 41 )

    ALLOCATE(tP(6) :: tIn( 2 ))

    OPEN(41, ACCESS='sequential', ACTION='read', FORM='unformatted')
    READ( 41 ) tIn( 1 )
    READ( 41 ) (tIn( i ), i = 2, 2)
    CLOSE( 41 )

    if (tIn( 1 )%c /= str(1)) error stop 1
    if (tIn( 2 )%c /= str(2)) error stop 2

END PROGRAM sequentialRead06
