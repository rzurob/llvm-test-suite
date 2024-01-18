! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/asynchIO/attribute/baseObj/baseObjExecStmt02.f
! opt variations: -qnol -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : baseObjExecStmt02 - ASYNCHRONOUS Attribute
!*                               in Scoping Unit
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 19, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Base Object appears in an
!*                               Executable Statement in a Scoping Unit
!*  SECONDARY FUNCTIONS TESTED : Any Statement of the Scoping Unit is
!*                               executed but the Variable is *NOT* a
!*                               Pending I/O Storage Sequence Affector
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Omit One Condition -- Derived Type, implicit Attribute, matches
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

MODULE mTimeStamp
    IMPLICIT NONE

    TYPE tTimeStamp(N1,K1)    ! (20,4)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        SEQUENCE

        INTEGER(K1)   :: year
        INTEGER(K1)   :: month
        INTEGER(K1)   :: dayOfMonth

        INTEGER(K1)   :: hours
        INTEGER(K1)   :: minutes
        INTEGER(K1)   :: seconds
    END TYPE tTimeStamp
END MODULE mTimeStamp


PROGRAM baseObjExecStmt02
    USE mTimeStamp

    IMPLICIT NONE

    INTEGER :: iStat

    TYPE(tTimeStamp(20,4)) :: aTimeStamp

    CHARACTER(len = 256) :: ioErrorMsg


    OPEN(8, ACTION='write', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "OPEN(): (", iStat, ") ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    !
    !  (1)  Structure Components of "aTimeStamp" appear in a "Executable
    !       Statement(s)" in the Scoping Unit.
    !
    aTimeStamp%year       = 2006
    aTimeStamp%month      = 1
    aTimeStamp%dayOfMonth = 19

    aTimeStamp%hours      = 18
    aTimeStamp%minutes    = 27
    aTimeStamp%seconds    = 15


    CALL AsynchronousWrite( aTimeStamp )


    !
    !  (2)  A statement of the Scoping Unit is executed while the Structure
    !       Components of "aTimeStamp" is a "Pending I/O Storage Sequence
    !       Affector".
    !
    !   ==> This condition is no longer .TRUE. as the Structure Components
    !       "aTimeStamp%[hours|minutes|seconds]" are no longer "Pending I/O
    !       Storage Sequence Affector(s)".
    !
    PRINT *, "iStat = (", iStat, ")"


    CLOSE(8, IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "CLOSE(): (", iStat, ") ", ioErrorMsg
        CALL zzrc( 4 )
    END IF


CONTAINS


    SUBROUTINE AsynchronousWrite( asynchTS )
        USE mTimeStamp

        IMPLICIT NONE

        INTEGER :: iStatus

        CHARACTER(len = 256) :: ioErrMsg

        TYPE(tTimeStamp(*,4)), ASYNCHRONOUS :: asynchTS


        !
        !  On completion of the Asynchronous WRITE() the Structure Components
        !  "asynchTS%[hours|minutes|seconds]", are considered to be "Pending
        !  I/O Storage Sequence Affector(s)".
        !
        WRITE(8, 10, ASYNCHRONOUS='yes', IOSTAT=iStatus, IOMSG=ioErrMsg)&
                &asynchTS%hours, asynchTS%minutes, asynchTS%seconds
        IF (iStatus /= 0) THEN
            PRINT *, "WRITE(): (", iStatus, ") ", ioErrMsg
            CALL zzrc( 2 )
        END IF

10      FORMAT(BZ,I2,':',I2,':',I2)


        !
        !  Wait for Asynchronous WRITE() to Complete.
        !
        !  Structure Components "asynchTS%[hours|minutes|seconds]" are
        !  no longer be considered to be "Pending I/O Storage Sequence
        !  Affector(s)".
        !
        WAIT(8, IOSTAT=iStatus, IOMSG=ioErrMsg)
        IF (iStatus /= 0) THEN
            PRINT *, "WAIT(): (", iStatus, ") ", ioErrMsg
            CALL zzrc( 3 )
        END IF

    END SUBROUTINE AsynchronousWrite
END PROGRAM baseObjExecStmt02
