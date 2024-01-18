! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_poly/associate/CrossFeatures/DerTypeComp.f
! opt variations: -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:  tcomp DerTypeComp.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : DerTypeComp
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
!*    Illegal usage on componet:
!*    Abstract/private componet as selector
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE, ABSTRACT :: Base(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: BaseId = 1
    END TYPE

  END MODULE

  MODULE M1
  USE M0, DT0=>Base

    TYPE, EXTENDS(DT0) :: Child    ! (20,4)
      PRIVATE
      INTEGER(K1)  :: ChildId = 2
    END TYPE

  END MODULE

  MODULE M
  USE M1, DT=>Child

  TYPE(DT(20,4)), SAVE :: T

  END MODULE

  PROGRAM DerTypeSeq
  USE M, V=>T
  IMPLICIT NONE


  ASSOCIATE( As => V )
    ASSOCIATE( As => As%Base )
    END ASSOCIATE

    ASSOCIATE( As => As%ChildID )
    END ASSOCIATE
  END ASSOCIATE


  END


