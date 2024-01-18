!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 17, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The basic syatax
!*  intrinsic-type-spec -- diagnostic
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecIntrinsic3

  INTEGER, PARAMETER :: KIND = 1
  INTEGER, PARAMETER :: LEN  = 1

  !these are fine
  INTEGER(KIND=KIND)  :: I2
  CHARACTER(LEN=LEN)  :: C


  INTEGER(LEN=4)  :: I1

  LOGICAL(LEN=1)  :: L1

  REAL(LEN=1)     :: R4

  COMPLEX(LEN=1)  :: Z4

  INTEGER(KIND=1, LEN=2)  :: I3

  DOUBLE PRECISION(KIND=8) :: D

  END

