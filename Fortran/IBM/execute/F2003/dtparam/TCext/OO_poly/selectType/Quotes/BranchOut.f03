! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/selectType/Quotes/BranchOut.f
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
!*  Brach out of select type construct
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


  PROGRAM BranchOut
  USE M
  IMPLICIT NONE

  TYPE(DT(20,4))  ::  DTV(3,3,3)

  CALL Sub(DTV)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(DT(*,4))  :: Arg(:,:,:)
  INTEGER :: S(3)=(/1,2,3/)

    GOTO 1
1   SELECT TYPE (U => Arg(:,S,:))
2   CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))        ERROR STOP 30
      IF ( SIZE(U)          .NE. 27 )         ERROR STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1/) ) ) ERROR STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3,3,3/) ) ) ERROR STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/3,3,3/)) )  ERROR STOP 34

    ASSOCIATE ( W => U )
      GOTO 3
3     SELECT TYPE (U => W )

4     TYPE IS (DT(*,4))

        IF ( ANY(U%Id      .NE. DTV%Id ) )      ERROR STOP 42
        IF ( ANY(U%GetId() .NE. DTV%GetId()))   ERROR STOP 43
        GOTO 9

5     CLASS DEFAULT
        STOP 51
6     END SELECT
9     PRINT*, "OK"
      GOTO 10

7   END ASSOCIATE
8   END SELECT
10  PRINT*, "OK"

  END SUBROUTINE

  END



