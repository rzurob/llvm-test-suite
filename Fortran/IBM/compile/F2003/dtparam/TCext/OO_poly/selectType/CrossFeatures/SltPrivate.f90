! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_poly/selectType/CrossFeatures/SltPrivate.f
! opt variations: -qck -qnok -ql -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp SltPrivate.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltPrivate
!*
!*  DATE                       : Jan. 28, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!*
!*  Diagnostic : The selector is a private  component
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE  :: DT0(K1,N1)    ! (4,1025)
      INTEGER, KIND          :: K1
      INTEGER, LEN           :: N1
      INTEGER(K1)            :: IArr(2)=-1
      CHARACTER(N1), PRIVATE :: CArr(2)="!"
    END TYPE

    TYPE, ABSTRACT :: DT1(K2)    ! (4)
      INTEGER, KIND                      :: K2
      CLASS(DT0(K2,:)), POINTER, PRIVATE :: Ptr
    END TYPE

    TYPE, EXTENDS(DT1) :: DT    ! (4)
      PRIVATE
    END TYPE

  END MODULE

  PROGRAM SltPrivate
  USE M
  IMPLICIT NONE
  TYPE(DT0(4,1025)), TARGET :: V

  TYPE (DT(4))  :: U

  CALL Sub(U)

  CONTAINS

  SUBROUTINE Sub(U)
  CLASS(DT1(4)) :: U

  SELECT TYPE(U)
  CLASS IS (DT(4))
    SELECT TYPE (U => U%DT1%Ptr)
    CLASS IS (DT0(4,*))
    END SELECT
  END SELECT

  END SUBROUTINE

  END



