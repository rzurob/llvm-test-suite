!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : d325937 - Order of Expression Evaluation
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September 27, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2003: ACE: ICE in ASTI with Implied-DO
!*                               using CHARACTER Substring
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Array Constructor, Implied-DO
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Affects:
!*  /tstdev/F2003/allocEnh/exprEval/order/arrayExprVectorIndex04.scenario
!*
!*  Error:
!*  !STEP1:F_COMPILE:RC=255!
!*
!*  Reduced Code below uses a CHARACTER Substring in the Implied-DO of an
!*  Array Constructor, which causes an ICE in asti:
!*
!*  chaos: compilation abandoned - unsupported kind of subtraction
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM arrayExprVectorIndex04

    CHARACTER(1), PARAMETER :: charVar = 'a'

    PRINT *, (/ (charVar( i:i ), i = 1, 1) /)

END PROGRAM arrayExprVectorIndex04
