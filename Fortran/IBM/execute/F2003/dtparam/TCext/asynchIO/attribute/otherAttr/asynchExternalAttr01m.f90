! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/asynchIO/attribute/otherAttr/asynchExternalAttr01m.f
! opt variations: -qnol -qreuse=none

!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February  9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the EXTERNAL Attribute
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Module "mPoint".
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
END MODULE mPoint
