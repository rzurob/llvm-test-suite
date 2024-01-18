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
! %POSTCMD:  tcomp ExecSeq2.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ExecSeq1
!*
!*  DATE                       : Jan. 27, 2005
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
!*  Diagnosis : Bad control sequence
!*  (ICE-298858)
!*
!234567890123456789012345678901234567890123456789012345678901234567890





  MODULE M
    TYPE :: DT
      INTEGER :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    CLASS(DT), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION
  END MODULE


  PROGRAM ExecSeq2
  USE M
  IMPLICIT NONE

  TYPE(DT), TARGET   ::  DTV(3,3,3)
  CLASS(DT), POINTER :: Ptr(:,:,:)
  INTEGER :: S(3)=(/1,2,3/), I, J

    Ptr => Dtv

    IF(1) 1, 1, 2

1   CONTINUE

    SELECT TYPE (U => Ptr(S,S,S))
2   CLASS DEFAULT

      SELECT TYPE (U => U )
      CLASS DEFAULT
      END SELECT

    END SELECT


  END


