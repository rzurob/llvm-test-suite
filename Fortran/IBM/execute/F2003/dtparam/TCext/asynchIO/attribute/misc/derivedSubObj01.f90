! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/asynchIO/attribute/misc/derivedSubObj01.f
! opt variations: -qnol -qreuse=none

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

    TYPE tPoint(N1,K1)    ! (20,4)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        REAL(K1)      :: x
        REAL(K1)      :: y
    END TYPE tPoint

    TYPE( tPoint(20,4) ), ASYNCHRONOUS :: asynchPoint


    asynchPoint%x = 5.125
    asynchPoint%y = 1.391

    PRINT 10, asynchPoint
10  FORMAT('asynchPoint%x = "',F5.3,'", asynchPoint%y = "',F5.3,'"')


END PROGRAM derivedSubObj01
