! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Cons/C810Misc3.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     - Wrong select type constructs
!*     (ICE when C810)
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C810Misc3
  IMPLICIT NONE

  TYPE :: T(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: i = 1
  END TYPE

  CLASS(T(4)), ALLOCATABLE :: V

  ALLOCATE(V)

  SELECT TYPE( As  => V)
  SELECT TYPE( As  => V)
  END SELECT

  END

