! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/selectType/Quotes/C815TypeIsSeq.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C815
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*   Use sequence type as type spec
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C815TypeIsSeq
  IMPLICIT NONE

  TYPE :: Seq(K1)    ! (4)
    INTEGER, KIND :: K1
    SEQUENCE
    INTEGER(K1)   :: i
  END TYPE

  TYPE(Seq(4)) :: Arg

  CALL Sub(Arg)

  CONTAINS

  SUBROUTINE Sub(Arg)

  CLASS(*) :: Arg

  SELECT TYPE ( Arg )
    TYPE IS (Seq(4))
    CLASS IS (Seq(4))
    CLASS DEFAULT
      STOP 30
  END SELECT

  END SUBROUTINE
  END
