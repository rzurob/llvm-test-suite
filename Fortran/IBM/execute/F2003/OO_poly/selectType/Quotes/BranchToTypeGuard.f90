! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: BranchToTypeGuard.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : BranchToTypeGuard
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
!*  Diagnostic: Brach to Typeguard Statement
!*  As the error message like
!*
!*  1511-122 (E) Unconditional GO TO statement refers to statement inside a DO-loop,
!*  IF block, CASE construct, SELECT TYPE construct, ASSOCIATE construct, WHERE constructi
!*  or FORALL construct with label 7.  Transfer of control into a DO-loop, IF block, i
!*  CASE construct, SELECT TYPE construct, ASSOCIATE construct, WHERE construct or FORALL
!*  construct is not permitted.

!*  the wrong goto statement will be ignored.
!*  Branch to TYPE GUARD statement
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


  PROGRAM BranchToTypeGuard
  USE M
  IMPLICIT NONE

  TYPE(DT)  ::  DTV(3,3,3)

  CALL Sub(DTV)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(DT)  :: Arg(:,:,:)
  INTEGER :: S(3)=(/1,2,3/)

    GOTO 6
6   SELECT TYPE (U => Arg(:,S,:))
1   CLASS DEFAULT
      PRINT*, "In CLASS DEFAULT"
      IF ( .NOT. SAME_TYPE_AS(U, Arg))        STOP 30
      IF ( SIZE(U)          .NE. 27 )         STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3,3,3/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/3,3,3/)) )  STOP 34
      GOTO 1

    ASSOCIATE ( W => U )

2     SELECT TYPE (U => W )

7     TYPE IS (DT)

        IF ( ANY(U%Id      .NE. DTV%Id ) )      STOP 42
        IF ( ANY(U%GetId() .NE. DTV%GetId()))   STOP 43
        GOTO 7

3     CLASS DEFAULT
        STOP 51
      END SELECT
      GOTO 3

4   END ASSOCIATE
5   END SELECT

  END SUBROUTINE

  END



