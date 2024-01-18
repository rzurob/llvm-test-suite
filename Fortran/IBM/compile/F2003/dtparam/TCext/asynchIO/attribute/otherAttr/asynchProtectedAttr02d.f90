! GB DTP extension using:
! ftcx_dtp -qreuse=none /tstdev/F2003/asynchIO/attribute/otherAttr/asynchProtectedAttr02d.f
! opt variations: -qck -qreuse=self

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchProtectedAttr02d - ASYNCHRONOUS Attribute
!*                               Interactions with Other Attributes
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : February 13, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the PROTECTED Attribute
!*
!*  DRIVER STANZA              : xlf2003
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
!*
!*  5.2.11 PROTECTED statement
!*
!*  R542 protected-stmt  is  PROTECTED [ :: ] entity-name-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mControlData
    IMPLICIT NONE

    TYPE tControlData(K1,K2,K3,N1)    ! (4,4,4,30)
        INTEGER, KIND :: K1,K2,K3
        INTEGER, LEN  :: N1
        INTEGER(K1)   :: recordNumber
        LOGICAL(K2)   :: recordInput
        INTEGER(K3)   :: counter
        CHARACTER(N1) :: fileName
    END TYPE tControlData

    TYPE(tControlData(4,4,4,30)), PROTECTED :: controlData =
            tControlData(4,4,4,30)(0, .TRUE., 0, 'asynchProtectedAttr02d.dat')

    CONTAINS
        SUBROUTINE IncrementCounter( )
            controlData%counter = controlData%counter + 1
        END SUBROUTINE IncrementCounter
END MODULE mControlData


PROGRAM asynchProtectedAttr02d
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


15  FORMAT('Current Record:      ',I5,'\n',&
          &'Reading Input:       ',L5,'\n',&
          &'Number of Records:   ',I5,'\n',&
          &'File Name:               ',a30)


    OPEN(23, FILE=controlData%fileName, ACTION='write',&
            &ASYNCHRONOUS='yes', IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "OPEN() <", oStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    CALL IncrementCounter( )


    WRITE(23, 15, IOSTAT=wStat, IOMSG=oMsg) controlData
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

END PROGRAM asynchProtectedAttr02d



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
