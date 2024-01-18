!*  ===================================================================
!*
!*                               Attribute in Scoping Unit
!*
!*  DATE                       : January 19, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Base Object appears in an
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
!*  Baseline Test Case #1 -- Intrinsic Type, explicit Attribute, matches
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

PROGRAM execStmtPending01
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE AsynchronousWrite( asynchInt )
            IMPLICIT NONE
            INTEGER, ASYNCHRONOUS :: asynchInt
        END SUBROUTINE AsynchronousWrite
    END INTERFACE

    INTEGER :: iStat

    INTEGER, ASYNCHRONOUS :: asynchInteger

    CHARACTER(len = 256) :: ioErrorMsg


    OPEN(8, ACTION='write', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "OPEN(): ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    !
    !  (1)  The variable "asynchInteger" appears in an "Executable Statement"
    !       in the Scoping Unit.
    !
    asynchInteger = 5


    CALL AsynchronousWrite( asynchInteger )


    !
    !  (2)  A statement of the Scoping Unit is executed while the Variable
    !       "asynchInteger" is a "Pending I/O Storage Sequence Affector".
    !
    PRINT *, "iStat = (", iStat, ")"


    CLOSE(8, IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "CLOSE(): ", ioErrorMsg
        CALL zzrc( 3 )
    END IF

END PROGRAM execStmtPending01


SUBROUTINE AsynchronousWrite( asynchInt )
    IMPLICIT NONE

    INTEGER :: iStatus

    INTEGER, ASYNCHRONOUS :: asynchInt

    CHARACTER(len = 256) :: ioErrMsg


    !
    !  On completion of the Asynchronous WRITE() the Variable "asynchInt",
    !  is considered to be a "Pending I/O Storage Sequence Affector".
    !
    WRITE(8, *, ASYNCHRONOUS='yes', IOSTAT=iStatus, IOMSG=ioErrMsg) asynchInt
    IF (iStatus /= 0) THEN
        PRINT *, "WRITE(): ", ioErrMsg
        CALL zzrc( 2 )
    END IF

END SUBROUTINE AsynchronousWrite
