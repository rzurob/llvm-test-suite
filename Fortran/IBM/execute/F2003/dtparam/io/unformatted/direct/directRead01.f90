!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : September 30, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Data Transfer from a DIRECT File
!*  SECONDARY FUNCTIONS TESTED : input-item-list is a Derived Type (with
!*                               Parameters)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To read data from a Direct file in a random order (determined at
!*  run-time).
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tMod
    IMPLICIT NONE

    TYPE tT(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        REAL(k) :: ra( l )
    END TYPE tT

END MODULE tMod


PROGRAM directRead01
    USE tMod

    IMPLICIT NONE

    INTEGER, PARAMETER :: L = 2
    INTEGER, PARAMETER :: M = 8
    INTEGER, PARAMETER :: N = 20

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: iostat

    INTEGER :: c
    INTEGER :: cr
    INTEGER :: cm

    INTEGER :: readOrder( (N / L) ) = -1

    CHARACTER(19) :: file
    CHARACTER(255) :: iomsg

    REAL(M) :: v( N )

    TYPE(tT(M,L)), DIMENSION( (N / L) ) :: t, tV


    !
    !  Data Preparation:
    !
    !  readOrder - Order in which records are read from the file
    !
    !  v - Values
    !  tV - v in tT Format
    !
    CALL SYSTEM_CLOCK(c, cr, cm)
    CALL RANDOM_SEED( PUT=[ c ] )
    CALL RANDOM_NUMBER( v )

    DO i = 1, SIZE( readOrder )
        j = MOD(INT( (v( i ) * 10.0_8) ), 10)

        DO WHILE (readOrder( (j + 1) ) /= -1)
            j = MOD((j + 1), 10)
        END DO

        readOrder( (j + 1) ) = i
    END DO

    PRINT *, 'Read Order:', readOrder
    PRINT *


    DO i = 1, SIZE( v )
        v( i ) = 1.0_8 / REAL(i, M)
        PRINT *, "v(", i, ") =", v( i )
    END DO

    PRINT *

    DO i = 1, SIZE( tV )
        j = ((i - 1) * L) + 1
        tV( i ) = tT(M,L)(v( j:(j + 1) ))
    END DO


    !
    !  Create the DataFile
    !
    k = M * L
    OPEN(7, FILE='directRead01.dat', ACTION='write',&
         ACCESS='direct', RECL=k, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 10
    END IF

    DO i = 1, (N / L)
        j = (i - 1) * L

        WRITE(7, REC=i, IOSTAT=iostat, IOMSG=iomsg) v( (j + 1):(j + l) )
        IF (iostat /= 0) THEN
            PRINT *,i,') WRITE(',iostat,') ',iomsg( 1:LEN_TRIM(iomsg))
            STOP 15
        END IF
    END DO

    CLOSE(7, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 20
    END IF


    !
    !  READ() t from Data File:  Direct Access, 16 byte Record Length
    !
    OPEN(7, FILE='directRead01.dat', ACTION='read',&
         ACCESS='direct', RECL=k, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 30
    END IF

    DO i = 1, SIZE( readOrder )
        j = readOrder( i )

        READ(7, REC=j, IOSTAT=iostat, IOMSG=iomsg) t( j )
        IF (iostat /= 0) THEN
            PRINT *, j, ') READ(', iostat, ') ', iomsg( 1:LEN_TRIM( iomsg ) )
            STOP 35
        END IF

        PRINT *, 't(', j, ') =	', t( j )%ra
        PRINT *, 'tV(', j, ') =	', tV( j )%ra
        PRINT *
    END DO

    CLOSE(7, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 40
    END IF


    !
    !  Verify:  t
    !
    IF ( ANY(t%ra( 1 ) /= tV%ra( 1 )) ) THEN
        STOP 50
    END IF

    IF ( ANY(t%ra( 2 ) /= tV%ra( 2 )) ) THEN
        STOP 55
    END IF

END PROGRAM directRead01
