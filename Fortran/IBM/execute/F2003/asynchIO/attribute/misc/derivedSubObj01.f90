!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : derivedSubObj01 - ASYNCHRONOUS
!*                               Attribute in Scoping Unit
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Base Object has ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Subobjects have ASYNCHRONOUS Attribute
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
!*  If an object has the ASYNCHRONOUS attribute, then all of its
!*  subobjects also have the ASYNCHRONOUS attribute.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM derivedSubObj01
    IMPLICIT NONE

    INTEGER :: iStat
    CHARACTER(len = 256) :: ioErrorMsg

    TYPE tPoint
        REAL :: x
        REAL :: y
    END TYPE tPoint

    TYPE( tPoint ), ASYNCHRONOUS :: asynchPoint


    asynchPoint%x = 5.125
    asynchPoint%y = 1.391

    PRINT 10, asynchPoint
10  FORMAT('asynchPoint%x = "',F5.3,'", asynchPoint%y = "',F5.3,'"')


END PROGRAM derivedSubObj01
