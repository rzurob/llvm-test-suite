! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/Target1.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 25, 2005
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
!*  The associating entity's optional attribute.
!*  Var Definition Context
!*  ()
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


  PROGRAM Optional1
  USE M
  IMPLICIT NONE

  TYPE(DT(20,4)),   TARGET   ::    DTV(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
  TYPE(DT(:,4)),   POINTER  :: DTVPtr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)

  CALL Sub()
  CALL Sub(DTV)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*), TARGET, OPTIONAL  :: Arg(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
  INTEGER :: S(2)=(/1,2/)

    IF ( .NOT. PRESENT(Arg)) RETURN

    SELECT TYPE (U => Arg(S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S))
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))        ERROR STOP 30
      IF ( SIZE(U)          .NE. 4 )          ERROR STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/) ) ) ERROR STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/) ) ) ERROR STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/)) )  ERROR STOP 34

    ASSOCIATE ( W => U )

      SELECT TYPE (U => W )

      TYPE IS (DT(*,4))
        DTVPtr => DTV
        IF ( ANY(U%Id      .NE. DTVPtr%Id ) )      ERROR STOP 42
        IF ( ANY(U%GetId() .NE. DTVPtr%GetId()))   ERROR STOP 43

        U%Id = 0  !Var Def Context
        DTVPtr => U
        DTVPtr%Id = 0 !OK
        PRINT*, Fun(U)

      CLASS DEFAULT
        STOP 51
      END SELECT

    END ASSOCIATE
    END SELECT

  END SUBROUTINE

  FUNCTION Fun(Arg)
  CLASS(*), INTENT(OUT) :: Arg(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
  LOGICAL :: Fun
    Fun = .TRUE.
  END FUNCTION

  END



