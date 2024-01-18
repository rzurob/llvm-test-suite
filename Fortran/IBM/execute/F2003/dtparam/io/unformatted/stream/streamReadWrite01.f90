!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : streamReadWrite01
!*  TEST CASE TITLE            : Unformatted Intrinsic Input/Output (with DTP)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October  3, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type (with Type Parameters) I/O
!*                               with a STREAM File
!*  SECONDARY FUNCTIONS TESTED : I/O Performed using Type-Bound Procedures
!*                               with no PASS arguments
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
!*  (using the NOPASS attribute):
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

            GENERIC :: ReadB => ReadB_4
            GENERIC :: WriteB => WriteB_4

            PROCEDURE, NOPASS :: ReadB_4
            PROCEDURE, NOPASS :: WriteB_4

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

END MODULE tBMod


PROGRAM streamReadWrite01
    USE tBMod

    IMPLICIT NONE

    INTEGER(4) :: i
    INTEGER(4) :: cbA( 5 )
    INTEGER(4) :: bA( 5 ) = [ (i, i = 1_4, 5_4) ]

    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    TYPE(tB(4,5)) :: b


    PRINT *, 'bA =', bA

    OPEN(11, ACCESS='stream', ACTION='readwrite', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, "): ", iomsg
        STOP 10
    END IF

    WRITE(11, IOSTAT=iostat, IOMSG=iomsg) bA
    IF (iostat /= 0) THEN
        PRINT *, "WRITE(", iostat, "): ", iomsg
        STOP 11
    END IF

    REWIND(11, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, "): ", iomsg
        STOP 12
    END IF


    iostat = b%ReadB(b, 11)
    IF (iostat /= 0) STOP 20

    REWIND(11, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, "): ", iomsg
        STOP 21
    END IF


    PRINT *, 'b%i1 =', b%i1

    IF ( ANY(b%i1 /= bA) ) STOP 30


    WRITE(11, IOSTAT=iostat, IOMSG=iomsg) [ (-1_2, i = 1_4, 10_4) ]
    IF (iostat /= 0) THEN
        PRINT *, "WRITE(", iostat, "): ", iomsg
        STOP 40
    END IF

    REWIND(11, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, "): ", iomsg
        STOP 41
    END IF


    iostat = b%WriteB(b, 11)
    IF (iostat /= 0) STOP 50

    REWIND(11, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, "): ", iomsg
        STOP 51
    END IF


    READ(11, IOSTAT=iostat, IOMSG=iomsg) cbA
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

    IF ( ANY(cbA /= bA) ) STOP 70

END PROGRAM streamReadWrite01
