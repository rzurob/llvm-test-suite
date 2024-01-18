!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchExternalAttr01b - ASYNCHRONOUS Attribute
!*                               Interactions with Other Attributes
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : February  9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the EXTERNAL Attribute
!*
!*  DRIVER STANZA              : xlf2003
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

    TYPE(tPoint) :: thePt
    ASYNCHRONOUS :: thePt

    COMMON /externalCommon/ thePt
    DATA thePt /tPoint(29.325 , 30.156)/

END BLOCK DATA externalBlockData
