!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : sequentialWrite03
!*
!*  DATE                       : October  6, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Data Transfer to a SEQUENTIAL File
!*  SECONDARY FUNCTIONS TESTED : output-item-list consists of Derived Types
!*                               (with Parameters) that are Containers for
!*                               other Derived Types (also with Type Parameters)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform Unformatted Sequential Output of Derived Types with up
!*  to 4 Levels of Inheritance.
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


PROGRAM sequentialWrite03
    USE c4

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: iostat

    INTEGER :: v( 2 )

    CHARACTER(255) :: iomsg

    CHARACTER(15) :: xlfortran = 'IBM XL-Fortran!'

    TYPE(tC1(3)) :: vC1( 5 )
    TYPE(tC2(4,3)) :: vC2
    TYPE(tC3(3,4)) :: vC3
    TYPE(tC4(4,3)) :: vC4( 3 )

    ! "IBM", " XL", "-Fo", "rtr", "an!"
    vC1 = [ (tC1(3)(xlfortran( i:(i + 2) )), i = 1, 15, 3) ]

    vC2%c = vC1( 1 )                                        ! "IBM"
    vC3%c%c%c = vC1( 2 )%c( 2: ) // vC1( 3 )%c( 2:2 )       ! "XLF"

    vC4( 1 ) = tC4(4,3)(vC3)                                ! "XLF"
    vC4( 2 )%c%c%c%c = vC1( 3 )%c( 3: ) // vC1( 4 )%c( :2 ) ! "ort"
    vC4( 3 )%c%c%c%c = vC1( 4 )%c( 3: ) // vC1( 5 )%c( :2 ) ! "ran"


    OPEN(14, ACCESS='sequential', ACTION='write', RECL=6,&
         FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, '): ', iomsg
        STOP 10
    END IF


    WRITE(14, IOSTAT=iostat, IOMSG=iomsg) vC2, vC3          ! "IBMXLF"
    IF (iostat /= 0) THEN
        PRINT *, 7, 'WRITE(', iostat, '): ', iomsg
        STOP 20
    END IF

    DO i = 1, 5, 2  ! "IBM XL", "-Fortr", "an!IBM"
        v = [ i, (i + 1) ]
        IF (i == 5) THEN
            v( 2 ) = 1
        END IF

        WRITE(14, IOSTAT=iostat, IOMSG=iomsg) vC1( v )
        IF (iostat /= 0) THEN
            PRINT *, i, 'WRITE(', iostat, '): ', iomsg
            STOP 40
        END IF
    END DO

    DO i = 1, 3  ! "XLFort", "ortran", "ranXLF"
        v = [ i, (i + 1) ]
        IF (i == 3) THEN
            v( 2 ) = 1
        END IF

        WRITE(14, IOSTAT=iostat, IOMSG=iomsg) vC4( v )
        IF (iostat /= 0) THEN
            PRINT *, i, 'WRITE(', iostat, '): ', iomsg
            STOP 50
        END IF
    END DO


    CLOSE(14, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, '): ', iomsg
        STOP 60
    END IF

END PROGRAM sequentialWrite03
