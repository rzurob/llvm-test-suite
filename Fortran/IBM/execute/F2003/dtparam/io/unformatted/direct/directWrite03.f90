!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : directWrite03
!*  TEST CASE TITLE            : Unformatted Intrinsic Input/Output (with DTP)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October  6, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Data Transfer to a DIRECT File
!*  SECONDARY FUNCTIONS TESTED : output-item-list consists of Derived Types
!*                               (with Parameters) that are Containers for
!*                               other Derived Types (also with Type Parameters)
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform Unformatted Direct Output of Derived Types with up to 4
!*  Levels of Containers
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE c1
    IMPLICIT NONE

    TYPE tC1(l)
        INTEGER, LEN :: l

        CHARACTER(l) :: c
    END TYPE tC1

END MODULE c1


MODULE c2
    USE c1

    IMPLICIT NONE

    TYPE tC2(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        TYPE(tC1(l)) :: c
    END TYPE tC2

END MODULE c2


MODULE c3
    USE c2

    IMPLICIT NONE

    TYPE tC3(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        TYPE(tC2(k,l)) :: c
    END TYPE tC3

END MODULE c3


MODULE c4
    USE c3

    IMPLICIT NONE

    TYPE tC4(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        TYPE(tC3(l,k)) :: c
    END TYPE tC4

END MODULE c4


PROGRAM directWrite03
    USE c4

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    CHARACTER(15) :: xlfortran = 'IBM XL-Fortran!'

    TYPE(tC1(3)) :: vC1( 5 )
    TYPE(tC2(4,3)) :: vC2
    TYPE(tC3(3,4)) :: vC3
    TYPE(tC4(4,3)) :: vC4( 3 )

    vC1 = [ (tC1(3)(xlfortran( i:(i + 2) )), i = 1, 15, 3) ]

    vC2%c = vC1( 1 )
    vC3%c%c%c = vC1( 2 )%c( 2: ) // vC1( 3 )%c( 2:2 )

    vC4( 1 ) = tC4(4,3)(vC3)
    vC4( 2 )%c%c%c%c = vC1( 3 )%c( 3: ) // vC1( 4 )%c( :2 )
    vC4( 3 )%c%c%c%c = vC1( 4 )%c( 3: ) // vC1( 5 )%c( :2 )


    OPEN(14, ACCESS='direct', ACTION='write',&
            RECL=3, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, '): ', iomsg
        STOP 10
    END IF


    WRITE(14, REC=7, IOSTAT=iostat, IOMSG=iomsg) vC3
    IF (iostat /= 0) THEN
        PRINT *, 7, 'WRITE(', iostat, '): ', iomsg
        STOP 20
    END IF

    DO i = 1, 5
        IF (i <= 3) THEN
            j = 7 + i
            WRITE(14, REC=j, IOSTAT=iostat, IOMSG=iomsg) vC4( i )
            IF (iostat /= 0) THEN
                PRINT *, j, 'WRITE(', iostat, '): ', iomsg
                STOP 30
            END IF
        END IF

        WRITE(14, REC=i, IOSTAT=iostat, IOMSG=iomsg) vC1( i )
        IF (iostat /= 0) THEN
            PRINT *, i, 'WRITE(', iostat, '): ', iomsg
            STOP 40
        END IF
    END DO

    WRITE(14, REC=6, IOSTAT=iostat, IOMSG=iomsg) vC2
    IF (iostat /= 0) THEN
        PRINT *, 6, 'WRITE(', iostat, '): ', iomsg
        STOP 50
    END IF


    CLOSE(14, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, '): ', iomsg
        STOP 60
    END IF

END PROGRAM directWrite03
