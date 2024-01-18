! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/selectType/Quotes/BranchToEnd.f
! opt variations: -qnol

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
!*
!*  the wrong goto statement will be ignored.
!*  Branch to the end of select type construct
!*  (Error Reference-298811)
!*  Now seg fault-050321
!*
!234567890123456789012345678901234567890123456789012345678901234567890





  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    CLASS(DT(*,4)), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION
  END MODULE


  PROGRAM BranchToEnd
  USE M
  IMPLICIT NONE

  TYPE(DT(20,4))  ::  DTV(3,3,3)

  CALL Sub(DTV)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(DT(*,4))  :: Arg(:,:,:)
  INTEGER :: S(3)=(/1,2,3/)

    GOTO 8
1   SELECT TYPE (U => Arg(:,S,:))
2   CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))        STOP 30
      IF ( SIZE(U)          .NE. 27 )         STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3,3,3/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/3,3,3/)) )  STOP 34
!     GOTO 6

    ASSOCIATE ( W => U )
     GOTO 6

3     SELECT TYPE (U => W )

4     TYPE IS (DT(*,4))
        GOTO 7

        IF ( ANY(U%Id      .NE. DTV%Id ) )      STOP 42
        IF ( ANY(U%GetId() .NE. DTV%GetId()))   STOP 43

5     CLASS DEFAULT
        STOP 51
6     END SELECT
      GOTO 8

7   END ASSOCIATE
8   END SELECT

  END SUBROUTINE

  END



