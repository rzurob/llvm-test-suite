! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/asynchIO/attribute/otherAttr/asynchProtectedAttr01.f
! opt variations: -qnock -qreuse=self

!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February 14, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the PROTECTED Attribute
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  5.1 Type declaration statements
!*
!*  R501 type-declaration-stmt  is  declaration-type-spec [ [ , attr-spec ]&
!*                                      &... :: ] entity-decl-list
!*
!*  R502 declaration-type-spec  is  intrinsic-type-spec
!*
!*  R503 attr-spec  is  access-spec
!*                  or  ALLOCATABLE
!*                  or  ASYNCHRONOUS
!*  ...
!*                  or  PROTECTED
!*
!*  5.1.2.12 PROTECTED attribute
!*
!*  The PROTECTED attribute imposes limitations on the usage of module
!*  entities.  Other than within the module in which an entity is given
!*  the PROTECTED attribute,
!*
!*      (1) if it is a nonpointer object, it is not definable, and
!*      (2) if it is a pointer, its association status shall not be changed
!*          except that it may become undefined if its target is deallocated
!*          other than through the pointer (16.4.2.1.3) or if its target
!*          becomes undefined by execution of a RETURN or END statement.
!*
!*  5.2.11 PROTECTED statement
!*
!*  R542 protected-stmt  is  PROTECTED [ :: ] entity-name-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mControlData
    IMPLICIT NONE

    TYPE tControlData(K1,K2,K3,K4,N1)    ! (4,4,4,1,28)
        INTEGER, KIND             :: K1,K2,K3,K4
        INTEGER, LEN              :: N1
        INTEGER(K1)               :: recordNumber
        LOGICAL(K2)               :: recordInput
        INTEGER(K3)               :: counter
        CHARACTER(kind=K4,len=N1) :: fileName
    END TYPE tControlData

    TYPE(tControlData(4,4,4,1,28)), PROTECTED :: controlData =&
            &tControlData(4,4,4,1,28)(0, .TRUE., 0, 'asynchProtectedAttr01.dat')

    CONTAINS
        SUBROUTINE IncrementCounter( )
            controlData%counter = controlData%counter + 1
        END SUBROUTINE IncrementCounter
END MODULE mControlData


PROGRAM asynchProtectedAttr01
    USE mControlData

    IMPLICIT NONE

    INTERFACE
        INTEGER FUNCTION ReportCounter( ioUnit )
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: ioUnit
        END FUNCTION ReportCounter
    END INTERFACE

    INTEGER :: oStat
    INTEGER :: wStat

    CHARACTER(LEN = 256) :: oMsg


    OPEN(23, FILE=controlData%fileName, ACTION='write',&
        &FORM='unformatted', ASYNCHRONOUS='yes', IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "OPEN() <", oStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    CALL IncrementCounter( )


    WRITE(23, IOSTAT=wStat, IOMSG=oMsg) controlData
    IF (wStat <> 0) THEN
        WRITE(0, *) "OPEN() <", wStat, "> ", oMsg

    ELSE
        CALL IncrementCounter( )
        wStat = ReportCounter( 23 )
    END IF


    CLOSE(23, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", oStat, "> ", oMsg
        CALL zzrc( 3 )

    ELSE IF (wStat <> 0) THEN
        CALL zzrc( 2 )
    END IF

END PROGRAM asynchProtectedAttr01



INTEGER FUNCTION ReportCounter( ioUnit )
    USE mControlData

    IMPLICIT NONE

    INTEGER :: wID
    INTEGER, INTENT(IN) :: ioUnit

    CHARACTER(LEN = 256) :: oMsg


    ASYNCHRONOUS controlData


    WRITE(UNIT=ioUnit, ASYNCHRONOUS='yes', ID=wID,&
            &IOSTAT=ReportCounter, IOMSG=oMsg) controlData
    IF (ReportCounter <> 0) THEN
        WRITE(0, *) "WRITE(asychronous) <", ReportCounter, "> ", oMsg
    END IF


    WAIT(UNIT=ioUnit, ID=wID, IOSTAT=ReportCounter, IOMSG=oMsg)
    IF (ReportCounter <> 0) THEN
        WRITE(0, *) "WAIT() <", ReportCounter, "> ", oMsg
    END IF

END FUNCTION ReportCounter