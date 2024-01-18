! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=none /tstdev/OO_poly/selectType/CrossFeatures/SltAbs.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qreuse=self

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
! %POSTCMD: tcomp SltAbs.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltAbs
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
!*  Diagnostic : The selector is a parent component of abstract type
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE  :: DT0(K1,N1)    ! (4,1025)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      SEQUENCE
      INTEGER(K1)   :: IArr(2)=-1
      CHARACTER(N1) :: CArr(2)="!"
    END TYPE

    TYPE, ABSTRACT :: DT1(K2,N2,N3)    ! (4,20,1025)
      INTEGER, KIND    :: K2
      INTEGER, LEN     :: N2,N3
      TYPE(DT0(K2,N3)) :: Seq
    END TYPE

    TYPE, EXTENDS(DT1) :: DT(K3,N4)    ! (4,20,1025,4,20)
        INTEGER, KIND :: K3
        INTEGER, LEN  :: N4
    END TYPE

  END MODULE

  PROGRAM SltAbs
  USE M
  IMPLICIT NONE
  TYPE (DT(4,20,1025,4,20))  :: U(2,2,2)

  CALL Sub(U)

  CONTAINS

  SUBROUTINE Sub(U)
  CLASS(DT1(4,*,*)) :: U(:,:,:)

  SELECT TYPE(U)
  CLASS IS (DT(4,*,*,4,*))
    SELECT TYPE (U%DT1)
    CLASS IS (DT1(4,*,*))
    END SELECT
  END SELECT

  END SUBROUTINE

  END



