! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/associate/CrossFeatures/DerTypeBnd.f
! opt variations: -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 07, 2005
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
!*     Type binding call as selector
!*    (ICE on rename-300859)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE, ABSTRACT :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
      CONTAINS
      PROCEDURE,nopass :: Bnd
    END TYPE

    CONTAINS

    FUNCTION Bnd(Arg)
    INTEGER :: Bnd, Arg
      Bnd =Arg
      PRINT*, "OK"
    END FUNCTION

  END MODULE

  MODULE M1
  USE M0, DT0=>Base

    TYPE, EXTENDS(DT0) :: Child    ! (4)
      PRIVATE
      INTEGER(K1)  :: ChildId = 2
    END TYPE

  END MODULE

  MODULE M
  USE M1, DT=>Child

  TYPE(DT(4)), SAVE :: T

  END MODULE

  PROGRAM DerTypeSeq
  USE M, V=>T
  IMPLICIT NONE


  ASSOCIATE( As => V )
    ASSOCIATE( As => As%Bnd(2222) )
      IF ( As .NE. 2222) ERROR STOP  20
    END ASSOCIATE

    ASSOCIATE( As => As%Bnd(22) )
      IF ( As .NE. 22) ERROR STOP  21
    END ASSOCIATE
  END ASSOCIATE


  END

