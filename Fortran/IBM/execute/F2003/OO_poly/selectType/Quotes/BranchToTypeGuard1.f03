! *********************************************************************
!*  ===================================================================
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

!*  the wrong goto statement will be ignored? Not ignored. Still execute it.
!*  Branch to TYPE GUARD statement of another select type construct  -- Even it is ok with noopt
!*  but cause problem with smp. Change it to a normal TC (319960)
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
    CLASS DEFAULT
      PRINT*, "In CLASS DEFAULT"
      IF ( .NOT. SAME_TYPE_AS(U, Arg))        ERROR STOP 30
      IF ( SIZE(U)          .NE. 27 )         ERROR STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1/) ) ) ERROR STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3,3,3/) ) ) ERROR STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/3,3,3/)) )  ERROR STOP 34
      GOTO 1

1   ASSOCIATE ( W => U )
      PRINT*, "In Associate Construct"
      GOTO 2

2     SELECT TYPE (U => W )

7     TYPE IS (DT)
        PRINT*, "In TYPE IS"
        IF ( ANY(U%Id      .NE. DTV%Id ) )      ERROR STOP 42
        IF ( ANY(U%GetId() .NE. DTV%GetId()))   ERROR STOP 43

3     CLASS DEFAULT
        STOP 51
      END SELECT

4   END ASSOCIATE
5   END SELECT

  END SUBROUTINE

  END


