!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : sequentialRead03
!*
!*  DATE                       : October 22, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Data Transfer from a SEQUENTIAL File
!*  SECONDARY FUNCTIONS TESTED : input-item-list is an Extended Derived Type
!*                               (with Parameters)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To read (and verify) Extended Derived Type data from a Sequential file.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod
    IMPLICIT NONE

    TYPE base(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        CHARACTER((k1 * 2)) :: lable
    END TYPE base

END MODULE bMod


PROGRAM sequentialRead03
    USE bMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: derived
        REAL(k1) :: a( (l1 * 5) )
    END TYPE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: iostat

    REAL(4), PARAMETER :: checkA( 100 ) = [ ((1.0_4 / REAL( i )), i = 1, 100) ]

    CHARACTER(8) :: lables( 4 )
    CHARACTER(20) :: file = 'sequentialRead03.dat'
    CHARACTER(255) :: iomsg

    TYPE(derived(5,4)) :: d( 4 )


    OPEN(22, FILE=file, ACCESS='sequential', ACTION='write',&
         FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(write,", file, ",", iostat, ") ", iomsg
        STOP 10
    END IF

    DO i = 1, 4
        j = ((i - 1) * 25) + 1
        WRITE(lables( i ), "('Lable-',I2)") i

        WRITE(22, IOSTAT=iostat, IOMSG=iomsg) lables( i ), checkA( j:(j + 24) )
        IF (iostat /= 0) THEN
            PRINT *, "WRITE(", iostat, ") ", iomsg
            STOP 11
        END IF
    END DO

    CLOSE(22, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, ") ", iomsg
        STOP 12
    END IF


    OPEN(11, FILE=file, ACCESS='sequential', ACTION='read',&
         FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(read,", file, ",", iostat, ") ", iomsg
        STOP 20
    END IF

    DO i = 1, 4
        READ(11, IOSTAT=iostat, IOMSG=iomsg) d( i )
        IF (iostat /= 0) THEN
            PRINT *, i, "READ(", iostat, ") ", iomsg
            STOP 21
        END IF
    END DO

    CLOSE(11, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, ") ", iomsg
        STOP 22
    END IF


    DO i = 1, 4
        j = ((i - 1) * 25) + 1
        IF (d( i )%lable /= lables( i )) THEN
            PRINT *, i, 'labels = "', lables, '"'
            PRINT *, i, 'd%lable = "', d( i )%lable, '"'
            STOP 30

        ELSE IF ( ANY(d( i )%a /= checkA( j:(j + 24) )) ) THEN
            PRINT *, i, 'checkA(', j, ':', (j + 24),&
                        ') = (', checkA( j:(j + 24)), ')'
            PRINT *, i, 'd%a = (', d( i )%a, ')'
            STOP 40
        END IF
    END DO

END PROGRAM sequentialRead03
