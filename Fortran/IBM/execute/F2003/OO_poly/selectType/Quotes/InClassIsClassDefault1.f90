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
!*  Within the CLASS DEFAULT and TYPE IS
!*  for intrinsic types
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InClassIsClassDefault1
  IMPLICIT NONE

  INTEGER(1)      :: Int(2:3,3:4)=8_2
  COMPLEX(8)      :: Cplx(2:3,3:4)=(-8.0_4, 8.0_4)
  LOGICAL(8)      :: L(2:3,3:4)=.true._2
  CHARACTER(1025) :: C(2:3,3:4)
  CHARACTER(1025) :: Str

  Str(:) = '!'
  C = Str


  CALL Sub(Int(2:3,3:4))
  CALL Sub(Cplx(2:3,3:4))
  CALL Sub(L(2:3,3:4))
  CALL Sub(C(2:3,3:4))

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(2:3,3:4)

    SELECT TYPE (U => Arg)
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))       STOP 30
      IF ( SIZE(U)          .NE. 4 )          STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   STOP 34

      SELECT TYPE ( V => Arg )

      TYPE IS (INTEGER(1))
        SELECT TYPE (U)
        TYPE IS (INTEGER(1))
          IF ( ANY(U   .NE. V) )        STOP 35
          IF ( KIND(U) .NE. KIND(V) )   STOP 36
        END SELECT

      TYPE IS (COMPLEX(8))
        SELECT TYPE (U)
        TYPE IS (COMPLEX(8))
          IF ( ANY(U   .NE. V ))        STOP 37
          IF ( KIND(U) .NE. KIND(V) )   STOP 38
        END SELECT

      TYPE IS (LOGICAL(8))
        SELECT TYPE (U)
        TYPE IS (LOGICAL(8))
          IF ( ANY(U   .NEQV. V ))        STOP 40
          IF ( KIND(U) .NE. KIND(V) )   STOP 41
        END SELECT

      TYPE IS (CHARACTER(*))
        SELECT TYPE (U)
        TYPE IS (CHARACTER(*))
          IF ( ANY(U   .NE. V ))        STOP 42
          IF ( KIND(U) .NE. KIND(V) )   STOP 43
        END SELECT

      CLASS DEFAULT
        STOP 51
      END SELECT

    END SELECT

  END SUBROUTINE

  END



