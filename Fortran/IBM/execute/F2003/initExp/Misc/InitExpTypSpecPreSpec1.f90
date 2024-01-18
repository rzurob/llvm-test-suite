!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 29, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  intrinsic-type-spec in pre-spec : real and complex
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  REAL(KIND=4_8) FUNCTION R4()
    R4 = 4
  END FUNCTION

  REAL(KIND=SIZE([(i,i=1,8)])) FUNCTION R8()
    R8 = 8
  END FUNCTION

  REAL(KIND=SIZE([(i,i=1,16)])) FUNCTION R6()
    R6 = 16.
  END FUNCTION


  COMPLEX(KIND=4_1) FUNCTION Z4()
    Z4 = (4., -4.)
  END FUNCTION

  COMPLEX(KIND=SIZE([(i,i=1,8)])) FUNCTION Z8()
    Z8 = (8., -8.)
  END FUNCTION

  COMPLEX(KIND=SIZE([(i,i=1,16)])) FUNCTION Z6()
    Z6 = (16., -16.)
  END FUNCTION


  PROGRAM InitExpTypSpecPreSpec1
  IMPLICIT NONE

  INTEGER :: I


  PROCEDURE(REAL(KIND=4_8)),                 POINTER :: R4PreSpec
  PROCEDURE(REAL(KIND=SIZE([(i,i=1,8)]))),   POINTER :: R8PreSpec
  PROCEDURE(REAL(KIND=SIZE([(i,i=1,16)]))),  POINTER :: R6PreSpec

  PROCEDURE(COMPLEX(KIND=4_1)),                 POINTER :: Z4PreSpec
  PROCEDURE(COMPLEX(KIND=SIZE([(i,i=1,8)]))),   POINTER :: Z8PreSpec
  PROCEDURE(COMPLEX(KIND=SIZE([(i,i=1,16)]))),  POINTER :: Z6PreSpec

  REAL(KIND=4_8)               , EXTERNAL :: R4
  REAL(KIND=SIZE([(i,i=1,8)])) , EXTERNAL :: R8
  REAL(KIND=SIZE([(i,i=1,16)])), EXTERNAL :: R6

  COMPLEX(KIND=4_1)               , EXTERNAL :: Z4
  COMPLEX(KIND=SIZE([(i,i=1,8)])) , EXTERNAL :: Z8
  COMPLEX(KIND=SIZE([(i,i=1,16)])), EXTERNAL :: Z6

  R4PreSpec => R4
  R8PreSpec => R8
  R6PreSpec => R6

  Z4PreSpec => Z4
  Z8PreSpec => Z8
  Z6PreSpec => Z6


  IF ( R4PreSpec() .NE. 4.  ) ERROR STOP 11
  IF ( R8PreSpec() .NE. 8.  ) ERROR STOP 12
  IF ( R6PreSpec() .NE. 16. ) ERROR STOP 13

  IF ( Z4PreSpec() .NE. (4., -4.))  ERROR STOP 21
  IF ( Z8PreSpec() .NE. (8., -8.))  ERROR STOP 22
  IF ( Z6PreSpec() .NE. (16.,-16.)) ERROR STOP 23

  END


