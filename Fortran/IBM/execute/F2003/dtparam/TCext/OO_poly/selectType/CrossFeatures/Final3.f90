! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/CrossFeatures/Final3.f
! opt variations: -qck -qnok -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Final3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Final3
!*
!*  DATE                       : Feb. 02, 2005
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
!* Finalization
!* (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890





  MODULE M
    TYPE  :: DT(K1,N1)    ! (4,513)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C0="0"
      CONTAINS
      Final :: FinalDT
    END TYPE

    LOGICAL :: Final(10) = .FALSE.
    INTEGER :: Index = 0

    CONTAINS

    SUBROUTINE FinalDT(Arg)
    TYPE(DT(4,*)) :: Arg
      Index = Index + 1
      Final(Index) = .TRUE.
    END SUBROUTINE

  END MODULE

  PROGRAM final3
  USE M
  IMPLICIT CLASS(*)(U)

  TYPE :: Test(K2)    ! (4)
    INTEGER, KIND :: K2
    LOGICAL(K2)   :: V(3)
  END TYPE

  LOGICAL :: V(SIZE((/DT(4,513)(),DT(4,513)(),DT(4,513)()/)))=.FALSE.

  !No Finalization shall heppen before the first executable stmta
  !since no function return in the specification expr.
  IF (ANY(Final .NEQV. .FALSE.)) STOP 20

  Final = .FALSE.
  CALL Sub(Test(4)(V=V))

  CONTAINS

  SUBROUTINE  Sub(UArg)

  SELECT TYPE ( UArg )
  CLASS IS (Test(4))
    IF ( ANY(UArg%V) ) STOP 30
  CLASS DEFAULT
    STOP 61
  END SELECT
  SELECT TYPE ( UArg )
  CLASS IS (Test(4))
    IF ( ANY(UArg%V) ) STOP 31
  CLASS DEFAULT
    STOP 62
  END SELECT
  !No finalization shall happen

  END SUBROUTINE

  END


