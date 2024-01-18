!*  ===================================================================
!*
!*                               Attribute in Scoping Unit
!*
!*  DATE                       : January 19, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Base Object appears in an
!*                               Executable Statement in a Scoping Unit
!*  SECONDARY FUNCTIONS TESTED : Any Statement of the Scoping Unit is
!*                               executed while the Variable is a Pending
!*                               I/O Storage Sequence Affector
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Baseline Test Case #5 -- Derived Type, explicit Attribute, matches
!*  both conditions.
!*
!*  5.1.2.3  ASYNCHRONOUS Attribute
!*
!*  The base object of a variable shall have the ASYNCHRONOUS attribute in
!*  a scoping unit if:
!*
!*  (1) the variable appears in an executable statement or specification
!*      expression in that scoping unit and
!*
!*  (2) any statement of the scoping unit is executed while the variable is
!*      a pending I/O storage sequence affector (9.5.1.4)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mPoint
    IMPLICIT NONE

    TYPE tPoint
        SEQUENCE

        REAL :: x
        REAL :: y
    END TYPE tPoint
END MODULE mPoint


PROGRAM execStmtPending05
    USE mPoint

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE AsynchronousWrite( aPtr )
            USE mPoint
            IMPLICIT NONE

            TYPE(tPoint), ASYNCHRONOUS :: aPtr
        END SUBROUTINE AsynchronousWrite
    END INTERFACE

    INTEGER :: iStat

    CHARACTER(len = 256) :: ioErrorMsg

    TYPE(tPoint), ASYNCHRONOUS :: asynchPtr


    OPEN(8, ACTION='write', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "OPEN(): ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    !
    !  (1)  The variable "asynchPtr" appears in an "Executable Statement"
    !       in the Scoping Unit.
    !
    asynchPtr%x = 3.2
    asynchPtr%y = 6.7


    CALL AsynchronousWrite( asynchPtr )


    !
    !  (2)  A statement of the Scoping Unit is executed while the Variable
    !       "asynchPtr" is a "Pending I/O Storage Sequence Affector".
    !
    PRINT *, "iStat = (", iStat, ")"


    CLOSE(8, IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "CLOSE(): ", ioErrorMsg
        CALL zzrc( 3 )
    END IF

END PROGRAM execStmtPending05


SUBROUTINE AsynchronousWrite( aPtr )
    USE mPoint

    IMPLICIT NONE

    INTEGER :: iStatus

    CHARACTER(len = 256) :: ioErrMsg

    TYPE(tPoint), ASYNCHRONOUS :: aPtr


    !
    !  On completion of the Asynchronous WRITE() the Variable "aPtr%x",
    !  is considered to be a "Pending I/O Storage Sequence Affector".
    !
    WRITE(8, *, ASYNCHRONOUS='yes', IOSTAT=iStatus, IOMSG=ioErrMsg) aPtr%x
    IF (iStatus /= 0) THEN
        PRINT *, "WRITE(): ", ioErrMsg
        CALL zzrc( 2 )
    END IF

END SUBROUTINE AsynchronousWrite
