! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/asynchIO/attribute/baseObj/baseObjPending04.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*                               in Scoping Unit
!*
!*  DATE                       : January 19, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Base Object does *NOT* appear in
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
!*  Omit Condition (1) -- Derived Type, implicit Attribute, matches
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

MODULE mPoint
    IMPLICIT NONE

        TYPE tPoint(K1)    ! (4)
            INTEGER, KIND :: K1
            SEQUENCE

            REAL(K1)      :: x
            REAL(K1)      :: y
        END TYPE tPoint
END MODULE mPoint


PROGRAM baseObjPending04
    USE mPoint

    IMPLICIT NONE

    INTEGER :: iStat

    CHARACTER(len = 256) :: ioErrorMsg

    TYPE(tPoint(4)) :: asynchPtr


    asynchPtr = tPoint(4)(4.3 , 1.6)


    OPEN(8, ACTION='write', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "OPEN(): ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    CALL AsynchronousWrite( asynchPtr )
    CALL UpdateValue( asynchPtr )


    CLOSE(8, IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "CLOSE(): ", ioErrorMsg
        CALL zzrc( 3 )
    END IF

END PROGRAM baseObjPending04


SUBROUTINE AsynchronousWrite( aPtr )
    USE mPoint

    IMPLICIT NONE

    INTEGER :: iStatus

    CHARACTER(len = 256) :: ioErrMsg

    TYPE(tPoint(4)) :: aPtr


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


SUBROUTINE UpdateValue( aP )
    USE mPoint

    IMPLICIT NONE

    INTEGER iValue

    TYPE(tPoint(4)), INTENT(OUT) :: aP


    !
    !  (1)  The variable "aP" appears in an "Executable Statement"
    !       (or "Specification Expression") in the Scoping Unit.
    !
    !  ==> This condition no longer applies.
    !


    !
    !  (2)  A statement of the Scoping Unit is executed while the Variable
    !       "aP" is a "Pending I/O Storage Sequence Affector".
    !
    iValue = 55

END SUBROUTINE UpdateValue
