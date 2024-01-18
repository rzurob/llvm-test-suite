!*  ===================================================================
!*
!*                               in Scoping Unit
!*
!*  DATE                       : January 19, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Base Object appears in an
!*                               Executable Statement in a Scoping Unit
!*  SECONDARY FUNCTIONS TESTED : Any Statement of the Scoping Unit is
!*                               executed but the Variable is *NOT* a
!*                               Pending I/O Storage Sequence Affector
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Omit One Condition -- Intrinsic Type, implicit Attribute, matches
!*  Condition (1) only.
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

PROGRAM baseObjExecStmt01
    IMPLICIT NONE

    INTEGER :: iStat

    COMPLEX :: asynchComplex

    CHARACTER(len = 256) :: ioErrorMsg


    OPEN(8, ACTION='write', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "OPEN(): (", iStat, ") ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    !
    !  (1)  The variable "asynchComplex" appears in an "Executable Statement"
    !       in the Scoping Unit.
    !
    asynchComplex = ( 37.5,42 )


    CALL AsynchronousWrite( asynchComplex )


    !
    !  (2)  A statement of the Scoping Unit is executed while the Variable
    !       "asynchComplex" is a "Pending I/O Storage Sequence Affector".
    !
    !   ==> This condition is no longer .TRUE. as "asynchComplex" is no
    !       longer a "Pending I/O Storage Sequence Affector".
    !
    PRINT *, "iStat = (", iStat, ")"


    CLOSE(8, IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "CLOSE(): (", iStat, ") ", ioErrorMsg
        CALL zzrc( 4 )
    END IF


CONTAINS


    SUBROUTINE AsynchronousWrite( aComplex )
        IMPLICIT NONE

        INTEGER :: iStatus

        COMPLEX, ASYNCHRONOUS :: aComplex

        CHARACTER(len = 256) :: ioErrMsg


        !
        !  On completion of the Asynchronous WRITE() the Variable "aComplex",
        !  is considered to be a "Pending I/O Storage Sequence Affector".
        !
        WRITE(8, *, ASYNCHRONOUS='yes', IOSTAT=iStatus, IOMSG=ioErrMsg)&
                &aComplex
        IF (iStatus /= 0) THEN
            PRINT *, "WRITE(): (", iStatus, ") ", ioErrMsg
            CALL zzrc( 2 )
        END IF


        !
        !  Wait for Asynchronous WRITE() to Complete.  "aComplex" will no
        !  longer be considered to be a "Pending I/O Storage Sequence
        !  Affector".
        !
        WAIT(8, IOSTAT=iStatus, IOMSG=ioErrMsg)
        IF (iStatus /= 0) THEN
            PRINT *, "WAIT(): (", iStatus, ") ", ioErrMsg
            CALL zzrc( 3 )
        END IF

    END SUBROUTINE AsynchronousWrite
END PROGRAM baseObjExecStmt01
