!*  ===================================================================
!*
!*                               in Scoping Unit
!*
!*  DATE                       : January 19, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Base Object does *NOT* appear in
!*                               an Executable Statement in a Scoping Unit
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
!*  Omit Condition (1) -- Intrinsic Type, implicit Attribute, matches
!*  Condition (2) only.  The Scoping Unit where this test will be done
!*  is the SUBROUTINE "UpdateValue()".
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

PROGRAM baseObjPending02
    IMPLICIT NONE

    INTEGER :: iStat

    LOGICAL :: asynchLogical

    CHARACTER(len = 256) :: ioErrorMsg


    asynchLogical = .FALSE.

    OPEN(8, ACTION='write', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "OPEN(): ", ioErrorMsg
        ERROR STOP 1
    END IF


    CALL AsynchronousWrite( asynchLogical )
    CALL UpdateValue( asynchLogical )


    CLOSE(8, IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "CLOSE(): ", ioErrorMsg
        ERROR STOP 3
    END IF

END PROGRAM baseObjPending02


SUBROUTINE AsynchronousWrite( aLogical )
    IMPLICIT NONE

    INTEGER :: iStatus

    LOGICAL :: aLogical

    CHARACTER(len = 256) :: ioErrMsg


    !
    !  On completion of the Asynchronous WRITE() the Variable "aLogical",
    !  is considered to be a "Pending I/O Storage Sequence Affector".
    !
    WRITE(8, *, ASYNCHRONOUS='yes', IOSTAT=iStatus, IOMSG=ioErrMsg) aLogical
    IF (iStatus /= 0) THEN
        PRINT *, "WRITE(): ", ioErrMsg
        ERROR STOP 2
    END IF

END SUBROUTINE AsynchronousWrite


SUBROUTINE UpdateValue( aLog )
    IMPLICIT NONE

    INTEGER iValue

    LOGICAL, INTENT(OUT) :: aLog


    !
    !  (1)  The variable "aLog" appears in an "Executable Statement"
    !       (or "Specification Expression") in the Scoping Unit.
    !
    !  ==> This condition no longer applies.
    !


    !
    !  (2)  A statement of the Scoping Unit is executed while the Variable
    !       "aLog" is a "Pending I/O Storage Sequence Affector".
    !
    iValue = 55

END SUBROUTINE UpdateValue
