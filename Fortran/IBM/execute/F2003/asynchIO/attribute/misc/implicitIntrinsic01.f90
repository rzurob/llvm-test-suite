!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : implicitIntrinsic01 - ASYNCHRONOUS
!*                               Attribute in Scoping Unit
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute Conferred Implicitly
!*  SECONDARY FUNCTIONS TESTED : Intrinsic Type
!*
!*  DRIVER STANZA              : xlf2003
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

PROGRAM implicitIntrinsic01
    IMPLICIT NONE

    INTEGER :: iStat
    COMPLEX :: implicitAsynchComplex
    CHARACTER(len = 256) :: ioErrorMsg


    OPEN(8, FILE='implicitIntrinsic01.dat', ACTION='read',&
            &ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "OPEN(): ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    READ(8, *, ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=ioErrorMsg)&
        &implicitAsynchComplex
    IF (iStat /= 0) THEN
        PRINT *, "READ(): ", ioErrorMsg
        CALL zzrc( 2 )
    END IF


    CLOSE(8, IOSTAT=iStat, IOMSG=ioErrorMsg)
    IF (iStat /= 0) THEN
        PRINT *, "CLOSE(): ", ioErrorMsg
        CALL zzrc( 3 )
    END IF


    PRINT "('implicitAsynchComplex = ( ',F5.3,',',F5.3,' )')",&
          &implicitAsynchComplex
    IF (implicitAsynchComplex /= ( 5.125,1.391 )) THEN
        CALL zzrc( 4 )
    END IF

END PROGRAM implicitIntrinsic01
