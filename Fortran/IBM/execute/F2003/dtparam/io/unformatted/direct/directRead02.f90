!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : directRead02
!*
!*  DATE                       : September 30, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Data Transfer from a DIRECT File
!*  SECONDARY FUNCTIONS TESTED : input-item-list is an Extended Derived
!*                               Type (with Parameters)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To read Extended Derived Type data from a Direct file in a random
!*  order (determined at run-time).
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tMod
    IMPLICIT NONE

    TYPE tT(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        REAL(k) :: ra( l )
    END TYPE tT

    TYPE, EXTENDS(tT) :: tE
        COMPLEX(k) :: c
    END TYPE tE

END MODULE tMod


PROGRAM directRead02
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

    INTEGER :: readOrder( (N / (L * 2)) ) = -1

    CHARACTER(19) :: file
    CHARACTER(255) :: iomsg

    REAL(M) :: v( N )

    TYPE(tE(M,L)), DIMENSION( (N / (L * 2)) ) :: e, eV


    !
    !  Data Preparation:
    !
    !  readOrder - Order in which records are read from the file
    !
    !  v - Values
    !  eV - v in eT Format
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

    DO i = 1, SIZE( eV )
        j = ((i - 1) * (L * 2)) + 1
        eV( i ) = tE(M,L)(v( j:(j + 1) ),CMPLX(v( (j + 2) ),v( (j + 3) ),M))
    END DO


    !
    !  Create the DataFile
    !
    k = M * (L * 2)
    OPEN(7, FILE='directRead02.dat', ACTION='write',&
         ACCESS='direct', RECL=k, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 10
    END IF

    DO i = 1, (N / (L * 2))
        j = (i - 1) * (L * 2)

        WRITE(7, REC=i, IOSTAT=iostat, IOMSG=iomsg) v( (j + 1):(j + (L * 2)) )
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
    !  READ() e from Data File:  Direct Access, 32 byte Record Length
    !
    OPEN(7, FILE='directRead02.dat', ACTION='read',&
         ACCESS='direct', RECL=k, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 30
    END IF

    DO i = 1, SIZE( readOrder )
        j = readOrder( i )

        READ(7, REC=j, IOSTAT=iostat, IOMSG=iomsg) e( j )
        IF (iostat /= 0) THEN
            PRINT *, j, ') READ(', iostat, ') ', iomsg( 1:LEN_TRIM( iomsg ) )
            STOP 35
        END IF

        PRINT *, 'e(', j, ')%ra =	', e( j )%ra
        PRINT *, 'eV(', j, ')%ra =	', eV( j )%ra
        PRINT *, 'e(', j, ')%c =	', e( j )%c
        PRINT *, 'eV(', j, ')%c =	', eV( j )%c
        PRINT *
    END DO

    CLOSE(7, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 40
    END IF


    !
    !  Verify:  e
    !
    IF ( ANY(e%ra( 1 ) /= eV%ra( 1 )) ) THEN
        STOP 210
    END IF

    IF ( ANY(e%ra( 2 ) /= eV%ra( 2 )) ) THEN
        STOP 215
    END IF

    IF ( ANY(e%c /= eV%c) ) THEN
        STOP 220
    END IF

END PROGRAM directRead02
