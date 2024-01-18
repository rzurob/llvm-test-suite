!*  ===================================================================
!*
!*  DATE                       : September 27, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : F2003: ACE: ICE in xlfcode with Implied-DO
!*                               in RESHAPE() Intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : RESHAPE() Intrinsic, Array Constructor,
!*                               Implied-DO
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
!*  The Reduced Code below calls the RESHAPE() Intrinsic passing the result
!*  from an Array Constructor with a single CHARACTER(1) element.  This
!*  code ICEs in xlfcode.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM arrayExprVectorIndex04

    PRINT *, RESHAPE((/ ('a', j = 1, 1) /), (/ 1 /))

END PROGRAM arrayExprVectorIndex04
