!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : streamReadWrite02
!*  TEST CASE TITLE            : Unformatted Intrinsic Input/Output (with DTP)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October  3, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Extended Derived Type (with Type Parameters)
!*                               I/O with a STREAM File
!*  SECONDARY FUNCTIONS TESTED : Extended Derived Type is also a Container
!*                               for the Base Type
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE, READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform Unformatted Stream File I/O with Type Bound Procedures
!*  (using the NOPASS attribute).  The Extended Derived Type refrences
!*  the Base Derived Type's Type Bound Procedures to perform the actual
!*  I/O:
!*
!*  o  Write raw data to file,
!*  o  Read back as Derived Type (via Type Bound Procedure),
!*  o  Write data as Derived Type (via Type Bound Procedure), and
!*  o  Read (and Verify) raw data from file.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tBMod
    IMPLICIT NONE

    TYPE tB(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        INTEGER(k1) :: i1( l1 )

        CONTAINS

            GENERIC :: ReadB => ReadB_4, ReadB_8
            GENERIC :: WriteB => WriteB_4, WriteB_8

            PROCEDURE, NOPASS :: ReadB_4
            PROCEDURE, NOPASS :: WriteB_4

            PROCEDURE, NOPASS :: ReadB_8
            PROCEDURE, NOPASS :: WriteB_8

    END TYPE tB

    CONTAINS

        INTEGER FUNCTION ReadB_4(b4, u)
            TYPE(tB(4,*)) :: b4
            INTEGER :: u

            CHARACTER(255) :: iomsg

            READ(u, IOSTAT=ReadB_4, IOMSG=iomsg) b4
            IF (ReadB_4 /= 0) THEN
                PRINT *, "ReadB_4():  READ(", ReadB_4, "): ", iomsg
            END IF

        END FUNCTION ReadB_4

        INTEGER FUNCTION WriteB_4(b4, u)
            TYPE(tB(4,*)) :: b4
            INTEGER :: u

            CHARACTER(255) :: iomsg

            WRITE(u, IOSTAT=WriteB_4, IOMSG=iomsg) b4
            IF (WriteB_4 /= 0) THEN
                PRINT *, "WriteB_4():  WRITE(", WriteB_4, "): ", iomsg
            END IF

        END FUNCTION WriteB_4

        INTEGER FUNCTION ReadB_8(b8, u)
            TYPE(tB(8,*)) :: b8
            INTEGER :: u

            CHARACTER(255) :: iomsg

            READ(u, IOSTAT=ReadB_8, IOMSG=iomsg) b8
            IF (ReadB_8 /= 0) THEN
                PRINT *, "ReadB_8():  READ(", ReadB_8, "): ", iomsg
            END IF

        END FUNCTION ReadB_8

        INTEGER FUNCTION WriteB_8(b8, u)
            TYPE(tB(8,*)) :: b8
            INTEGER :: u

            CHARACTER(255) :: iomsg

            WRITE(u, IOSTAT=WriteB_8, IOMSG=iomsg) b8
            IF (WriteB_8 /= 0) THEN
                PRINT *, "WriteB_8():  WRITE(", WriteB_8, "): ", iomsg
            END IF

        END FUNCTION WriteB_8

END MODULE tBMod


MODULE tEMod
    USE tBMod

    IMPLICIT NONE

    TYPE, EXTENDS(tB) :: tE(k2)
        INTEGER, KIND :: k2

        TYPE(tB(k2,l1)) :: eB

        CONTAINS

            GENERIC :: ReadE => ReadE_48
            GENERIC :: WriteE => WriteE_48

            PROCEDURE, NOPASS :: ReadE_48
            PROCEDURE, NOPASS :: WriteE_48

    END TYPE tE

    CONTAINS

        INTEGER FUNCTION ReadE_48(e48, u)
            TYPE(tE(4,*,8)) :: e48
            INTEGER :: u

            ReadE_48 = e48%tB%ReadB(e48%tb, u)
            IF (ReadE_48 == 0) THEN
                ReadE_48 = e48%eB%ReadB(e48%eB, u)
            END IF

        END FUNCTION ReadE_48

        INTEGER FUNCTION WriteE_48(e48, u)
            TYPE(tE(4,*,8)) :: e48
            INTEGER :: u

            WriteE_48 = e48%tB%WriteB(e48%tB, u)
            IF (WriteE_48 == 0) THEN
                WriteE_48 = e48%eB%WriteB(e48%eB, u)
            END IF

        END FUNCTION WriteE_48

END MODULE tEMod


PROGRAM streamReadWrite02
    USE tEMod

    IMPLICIT NONE

    INTEGER(4) :: i
    INTEGER(4) :: cbA( 5 ) = -99
    INTEGER(4) :: bA( 5 ) = [ (i, i = 1_4, 5_4) ]

    INTEGER(8) :: j
    INTEGER(8) :: ceA( 5 ) = -99
    INTEGER(8) :: eA( 5 ) = [ (j, j = 1_8, 5_8) ]

    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    TYPE(tB(4,5)) :: b
    TYPE(tE(4,5,8)) :: e


    PRINT *, 'bA =', bA
    PRINT *, 'eA =', eA
    PRINT *

    OPEN(11, ACCESS='stream', ACTION='readwrite', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, "): ", iomsg
        STOP 10
    END IF

    WRITE(11, IOSTAT=iostat, IOMSG=iomsg) bA, eA
    IF (iostat /= 0) THEN
        PRINT *, "WRITE(", iostat, "): ", iomsg
        STOP 11
    END IF

    REWIND(11, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, "): ", iomsg
        STOP 12
    END IF


    iostat = e%ReadE(e, 11)
    IF (iostat /= 0) STOP 20

    REWIND(11, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, "): ", iomsg
        STOP 21
    END IF


    PRINT *, 'e%i1 =', e%i1
    PRINT *, 'e%eB%i1 =', e%eB%i1
    PRINT *

    IF ( ANY(e%i1 /= bA) ) STOP 30
    IF ( ANY(e%eB%i1 /= eA) ) STOP 31


    WRITE(11, IOSTAT=iostat, IOMSG=iomsg)&
        [ (-1_8, i = 1_4, 5_4) ], [ (-1_4, i = 1_4, 5_4) ]
    IF (iostat /= 0) THEN
        PRINT *, "WRITE(", iostat, "): ", iomsg
        STOP 40
    END IF

    REWIND(11, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, "): ", iomsg
        STOP 41
    END IF


    iostat = e%WriteE(e, 11)
    IF (iostat /= 0) STOP 50

    REWIND(11, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, "): ", iomsg
        STOP 51
    END IF


    READ(11, IOSTAT=iostat, IOMSG=iomsg) cbA, ceA
    IF (iostat /= 0) THEN
        PRINT *, "WRITE(", iostat, "): ", iomsg
        STOP 60
    END IF

    CLOSE(11, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, "): ", iomsg
        STOP 61
    END IF


    PRINT *, 'cbA =', cbA
    PRINT *, 'ceA =', ceA

    IF ( ANY(cbA /= bA) ) STOP 70
    IF ( ANY(ceA /= eA) ) STOP 71

END PROGRAM streamReadWrite02
