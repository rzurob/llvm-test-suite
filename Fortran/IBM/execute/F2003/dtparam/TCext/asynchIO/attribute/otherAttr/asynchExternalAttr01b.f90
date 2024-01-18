! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/asynchIO/attribute/otherAttr/asynchExternalAttr01b.f
! opt variations: -qnol -qreuse=none

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
!*  External Block Data Unit:  "externalBlockData".
!*
!234567890123456789012345678901234567890123456789012345678901234567890

BLOCK DATA externalBlockData
    USE mPoint

    TYPE(tPoint(20,4)) :: thePt
    ASYNCHRONOUS :: thePt

    COMMON /externalCommon/ thePt
    DATA thePt /tPoint(20,4)(29.325 , 30.156)/

END BLOCK DATA externalBlockData
