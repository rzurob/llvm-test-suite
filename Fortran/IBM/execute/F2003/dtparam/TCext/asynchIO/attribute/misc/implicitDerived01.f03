! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/asynchIO/attribute/misc/implicitDerived01.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*                               Attribute in Scoping Unit
!*
!*  DATE                       : January 17, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute Conferred Implicitly
!*  SECONDARY FUNCTIONS TESTED : Derived Type
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
!*  The ASYNCHRONOUS attribute may be conferred implicitly by the use of
!*  a variable in an asynchronous input/output statement (9.5.1.4).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM implicitDerived01
    IMPLICIT NONE

    INTEGER :: iStat
    CHARACTER(len = 256) :: ioErrorMsg

    TYPE tPoint(K1)    ! (4)
        INTEGER, KIND :: K1
        REAL(K1)      :: x
        REAL(K1)      :: y
    END TYPE tPoint

    TYPE( tPoint(4) ) :: implicitAsynchPoint


    implicitAsynchPoint%x = 5.125
    implicitAsynchPoint%y = 1.391


    OPEN(8, FILE='implicitDerived01.dat', ACTION='write',&
            &ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "OPEN(): ", ioErrorMsg
        ERROR STOP 1
    END IF


    WRITE(8, 10, ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrorMsg)&
        &implicitAsynchPoint
    IF (iStat /= 0) THEN
        PRINT *, "WRITE(): ", ioErrorMsg
        ERROR STOP 2
    END IF

10  FORMAT('implicitAsynchPoint%x = "',F5.3,'"',&
          &'implicitAsynchPoint%y = "',F5.3,'"')


    CLOSE(8, IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "CLOSE(): ", ioErrorMsg
        ERROR STOP 3
    END IF

END PROGRAM implicitDerived01
