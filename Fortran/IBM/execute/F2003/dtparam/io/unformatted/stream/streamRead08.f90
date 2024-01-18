!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : streamRead08
!*
!*  DATE                       : November  4, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : STREAM File Input for a Derived/Extended
!*                               Derived Type (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : Derived Types are ALLOCATED POINTERs
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted READ of a Derived Type/Extended Derived Type
!*  from a Stream file, and confirm the contents.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE n

    IMPLICIT NONE

    TYPE tN
        REAL(4) :: r( 6 )
    END TYPE tN

    TYPE, EXTENDS(tN) :: eN
        COMPLEX(8) :: c
    END TYPE eN

END MODULE n


MODULE d

    IMPLICIT NONE

    TYPE tP(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        REAL((k1 / 2)) :: r( (3 * l1) )
    END TYPE tP

    TYPE, EXTENDS(tP) :: eP
        COMPLEX(k1) :: c
    END TYPE eP

END MODULE d


PROGRAM streamRead08
    USE d, base => tP, extended => eP
    USE n

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k

    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    REAL(8), PARAMETER :: ra8( 24 ) = [ ((1.0_8 / REAL(i, 8)), i = 1, 24) ]
    REAL(4), PARAMETER :: ra4( 18 ) = [ REAL(ra8( 1:18 ), 4) ]

    COMPLEX(8), PARAMETER :: ca8( 3 ) =&
                [ (CMPLX(ra8( i ), ra8( (i + 1) ), 8), i = 19, 24, 2) ]

    CHARACTER(6) :: str( 3 ) = [ 'Hello!', 'to the', 'World!' ]

    INTEGER :: ioLength( 3 )
    INTEGER(4) :: array( 3,6 ) = RESHAPE([ (i, i = 1, 18) ], [ 3,6 ])

    TYPE(tN) :: tOut( 3 )
    TYPE(base(:,8)), POINTER :: tIn( : )

    TYPE(eN) :: eOut( 3 )
    TYPE(extended(:,8)), POINTER :: eIn( : )


    DO i = 1, 3
        j = ((i - 1) * 6) + 1

        tOut( i ) = tN(ra4( j:( j + 5 ) ))
        eOut( i ) = eN(ra4( j:( j + 5 ) ),ca8( i ))
    END DO


    OPEN(41, ACCESS='stream', ACTION='write', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( 10_4 )
    END IF


    DO i = 1, 3
        WRITE(41, IOSTAT=iostat, IOMSG=iomsg) eOut( i )
        IF (iostat /= 0) THEN
            PRINT *, 'WRITE(', iostat, ') ', iomsg
            CALL zzrc( (20_4 + INT(i, 4)) )
        END IF

        WRITE(41, IOSTAT=iostat, IOMSG=iomsg) tOut( i )
        IF (iostat /= 0) THEN
            PRINT *, 'WRITE(', iostat, ') ', iomsg
            CALL zzrc( (30_4 + INT(i, 4)) )
        END IF
    END DO


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( 40_4 )
    END IF


    ALLOCATE(base(2,8) :: tIn( 3 ))
    ALLOCATE(extended(2,8) :: eIn( 3 ))


    OPEN(41, ACCESS='stream', ACTION='read',&
            FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( 50_4 )
    END IF


    DO i = 1, 3
        READ(41, IOSTAT=iostat, IOMSG=iomsg) eIn( i )
        IF (iostat /= 0) THEN
            PRINT *, 'READ(', iostat, ') ', iomsg
            CALL zzrc( (60_4 + INT(i, 4)) )
        END IF

        READ(41, IOSTAT=iostat, IOMSG=iomsg) tIn( i )
        IF (iostat /= 0) THEN
            PRINT *, 'READ(', iostat, ') ', iomsg
            CALL zzrc( (70_4 + INT(i, 4)) )
        END IF
    END DO


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( 80_4 )
    END IF


    DO i = 1, 3
        j = ((i - 1) * 6) + 1

        IF ( ANY(tIn( i )%r /= ra4( j:( j + 5 ) )) ) THEN
            CALL zzrc( (100_4 + INT(i, 4)) )

        ELSE IF ( ANY(eIn( i )%r /= ra4( j:( j + 5 ) )) ) THEN
            CALL zzrc( (110_4 + INT(i, 4)) )

        ELSE IF (eIn( i )%c /= ca8( i )) THEN
            CALL zzrc( (120_4 + INT(i, 4)) )
        END IF
    END DO

END PROGRAM streamRead08
