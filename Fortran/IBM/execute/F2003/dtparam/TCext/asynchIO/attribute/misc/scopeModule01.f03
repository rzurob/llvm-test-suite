! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/asynchIO/attribute/misc/scopeModule01.f
! opt variations: -qnol -qreuse=none

!*  ===================================================================
!*
!*                               Attribute in Scoping Unit
!*
!*  DATE                       : January 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Base Object explicitly has
!*                               ASYNCHRONOUS Attribute in one Scoping Unit
!*  SECONDARY FUNCTIONS TESTED : Derived Base Object does *NOT* have the
!*                               ASYNCHRONOUS Attribute in another Scoping Unit
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  5.1.2.3  ASYNCHRONOUS Attribute
!*
!*  An object may have the ASYNCHRONOUS attribute in a particular scoping
!*  unit without necessarily having it in other scoping units (11.2.1,
!*  16.4.1.3).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mPoint
    IMPLICIT NONE

    TYPE tPoint(N1,K1)    ! (20,4)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        SEQUENCE

        REAL(K1)      :: x
        REAL(K1)      :: y
    END TYPE tPoint

    TYPE( tPoint(20,4) ) :: aPtr
END MODULE mPoint


PROGRAM scopeModule01
    USE mPoint

    IMPLICIT NONE


    aPtr = tPoint(20,4)( 5.7,9.1 )
    PRINT *, "Before:  aPtr%x = (", aPtr%x, "), aPtr%y = (", aPtr%y, ")"

    CALL AsynchronousWrite( )

    PRINT *, "After:   aPtr%x = (", aPtr%x, "), aPtr%y = (", aPtr%y, ")"

END PROGRAM scopeModule01


SUBROUTINE AsynchronousWrite( )
    USE mPoint

    IMPLICIT NONE

    INTEGER :: iStat

    CHARACTER(len = 256) :: ioErrMsg

    ASYNCHRONOUS :: aPtr


    OPEN(8, ACTION='write', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrMsg)
    IF (iStat /= 0) THEN
        PRINT *, "OPEN(): ", ioErrMsg
        CALL zzrc( 1 )
    END IF


    aPtr%x = 3.2
    aPtr%y = 6.7


    WRITE(8, *, ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrMsg)&
        &aPtr%x
    IF (iStat /= 0) THEN
        PRINT *, "WRITE(): ", ioErrMsg
        CALL zzrc( 2 )
    END IF


    CLOSE(8, IOSTAT=iStat, IOMSG=ioErrMsg)
    IF (iStat /= 0) THEN
        PRINT *, "CLOSE(): ", ioErrMsg
        CALL zzrc( 3 )
    END IF


END SUBROUTINE AsynchronousWrite