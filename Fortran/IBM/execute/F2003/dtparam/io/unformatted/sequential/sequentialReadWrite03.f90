!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : sequentialReadWrite03
!*  TEST CASE TITLE            : Unformatted Intrinsic Input/Output (with DTP)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October  8, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SEQUENTIAL File I/O for an array of Derived
!*                               Type using an io-implied-do
!*  SECONDARY FUNCTIONS TESTED : Derived Type consists of multiple levels
!*                               of Containers where the Length Parameters
!*                               are used in expressions
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE, READ, io-implied-do
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted Sequential Write/Read of a Derived Type
!*  Array which is a Container of another Derived Type.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE r
    IMPLICIT NONE

    INTEGER, PARAMETER :: N = 32

    TYPE tR(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        REAL(k) :: ra( ((N / k) * l) )
    END TYPE tR

END MODULE r


MODULE c1
    USE r

    IMPLICIT NONE

    TYPE tC1(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        TYPE(tR((l / 5),(k / 5))) :: r
    END TYPE tC1

END MODULE c1


MODULE c2
    USE c1

    IMPLICIT NONE

    TYPE tC2(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        TYPE(tC1((l * 5),(k * 5))) :: c1
    END TYPE tC2

END MODULE c2


MODULE c3
    USE c2

    IMPLICIT NONE

    TYPE tC3(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        TYPE(tC2(l,k)) :: c2
    END TYPE tC3

END MODULE c3


PROGRAM sequentialReadWrite03
    USE c3

    IMPLICIT NONE

    INTERFACE
        INTEGER FUNCTION Write8(u, dt)
            USE c3
            INTEGER :: u
            TYPE(tC3(*,8)) :: dt( : )
        END FUNCTION Write8

        INTEGER FUNCTION Read8(u, dt)
            USE c3
            INTEGER :: u
            TYPE(tC3(*,8)) :: dt( : )
        END FUNCTION Read8
    END INTERFACE

    INTEGER(4) :: i
    INTEGER :: j, k, l, iostat

    CHARACTER(255) :: iomsg

    TYPE(tC3(1,8)) :: cC3( 5 )
    TYPE(tC3(:,8)), ALLOCATABLE :: vC3( : )


    DO i = 1, 5
        cC3( i )%c2%c1%r%ra = [ ((REAL(i, 8) / REAL(j, 8)), j = i, (i + 4)) ]
    END DO


    OPEN(8, ACTION='readwrite', FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, ") ", iomsg
        CALL zzrc( 10_4 )
    END IF


    iostat = Write8(8, cC3)
    IF (iostat /= 0) CALL zzrc( 20_4 )


    REWIND(8, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, ") ", iomsg
        CALL zzrc( 30_4 )
    END IF


    READ(8, IOSTAT=iostat, IOMSG=iomsg) l, i
    IF (iostat /= 0) THEN
        PRINT *, "READ(", iostat, ") ", iomsg
        CALL zzrc( 40_4 )
    END IF

    ALLOCATE(tC3(l,8) :: vC3( i ))

    iostat = Read8(8, vC3)
    IF (iostat /= 0) CALL zzrc( 50_4 )


    CLOSE(8, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, ") ", iomsg
        CALL zzrc( 60_4 )
    END IF


    DO i = 1_4, 5_4
        IF ( ANY(cC3( i )%c2%c1%r%ra /= vC3( i )%c2%c1%r%ra) ) THEN
            CALL zzrc( (70_4 + i) )
        END IF
    END DO

END PROGRAM sequentialReadWrite03


INTEGER FUNCTION Write8(u, dt)
    USE c3

    IMPLICIT NONE

    INTEGER :: u
    TYPE(tC3(*,8)) :: dt( : )

    INTEGER :: i
    CHARACTER(255) :: iomsg


    write (u) dt%l, SIZE( dt )
    do i = 1, size(dt)
        WRITE(u, IOSTAT=Write8, IOMSG=iomsg) dt( i )

        IF (Write8 /= 0) THEN
            PRINT *, "Write8():  WRITE(", Write8, ") ", iomsg
            stop 110
        END IF
    end do

END FUNCTION Write8


INTEGER FUNCTION Read8(u, dt)
    USE c3

    IMPLICIT NONE

    INTEGER :: u
    TYPE(tC3(*,8)) :: dt( : )

    INTEGER :: i
    CHARACTER(255) :: iomsg


    do i = 1, size(dt)
        READ(u, IOSTAT=Read8, IOMSG=iomsg) dt( i )

        IF (Read8 /= 0) THEN
            PRINT *, "Read8():  READ(", Read8, ") ", iomsg
            stop 111
        END IF
    end do

END FUNCTION Read8
