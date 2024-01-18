!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 23, 2006
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
!*  a reference to an elemental intrinsic
!*
!*  -ASIN
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemASIN
  IMPLICIT INTEGER(R)
  INTEGER :: I, J, K

  INTERFACE

  FUNCTION ExtFun1(Arg)
    COMPLEX(NINT(8*ASIN(0.84147098))) :: Arg(NINT(ASIN(0.84147098))+5)
    COMPLEX(NINT(8*ASIN(0.84147098))) :: ExtFun1(NINT(6*ASIN(0.84147098)))
  END FUNCTION

  END INTERFACE

  COMPLEX(NINT(8*ASIN(0.84147098))), PARAMETER :: T(NINT(ASIN(0.84147098))+5)= &
             (/((ASIN(r/10.),ASIN(-r/10.)), r=1, 6, 1)/)

  COMPLEX(NINT(8*ASIN(0.84147098))), PARAMETER :: T1(NINT(ASIN(0.84147098))+5)= T

  IF (ANY(ABS(ExtFun1(T)-T)    .GE.  1.0e-7     ) )           STOP 11
  IF (ANY(ABS(ExtFun1(T1)-T1)  .GE.  1.0e-7     ) )           STOP 12

  END

  FUNCTION ExtFun1(Arg)
  COMPLEX(NINT(8*ASIN(0.84147098))) :: Arg(NINT(ASIN(0.84147098))+5)
  COMPLEX(NINT(8*ASIN(0.84147098))) :: ExtFun1(NINT(6*ASIN(0.84147098)))
    ExtFun1 = Arg
  END FUNCTION


