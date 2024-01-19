!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February  9, 2006
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

    TYPE tPoint
        SEQUENCE

        REAL :: x
        REAL :: y
    END TYPE tPoint
END MODULE mPoint
