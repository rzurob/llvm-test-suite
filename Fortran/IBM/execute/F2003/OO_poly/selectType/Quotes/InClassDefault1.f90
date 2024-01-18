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
!*  Within the CLASS DEFAULT, the associating entity is polymorphic
!*  and has the same declared type as the selector
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
      INTEGER(1) :: Int=8_1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      COMPLEX(8) :: Cplx=(-8.0_8, 8.0_8)
      LOGICAL(8) :: L=.true._8
      CHARACTER(1025) :: C
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    END TYPE

  END MODULE


  PROGRAM InClassDefault1
  USE M
  IMPLICIT NONE
  TYPE(Base) :: V(2:3,3:4)
  CHARACTER(1025) :: Str

  Str(:) = '!'
  V%C(:) = Str
  CALL Sub(V(2:3,3:4))

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(2:3,3:4)
  TYPE(Base) :: T
  INTEGER :: i

    SELECT TYPE (U => Arg)
    CLASS DEFAULT
      SELECT TYPE (U)
      CLASS IS (Base)
        T%L = .FALSE.
        T = U(2,3)
        IF ( .NOT. T%L )            STOP 20
        IF ( LEN(U%C) .NE. 1025 )   STOP 21
        IF ( ANY(U%C  .NE. Str) )   STOP 22

        IF ( .NOT. SAME_TYPE_AS(U, Arg))       STOP 30
        IF ( SIZE(U)          .NE. 4 )          STOP 31
        IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) STOP 32
        IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) STOP 33
        IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   STOP 34

        IF ( ANY(U%Int        .NE. 8_1) )      STOP 35
        IF ( KIND(U%Int) .NE. 1 )              STOP 36

        IF ( ANY(U%Cplx       .NE. (-8.0_8, 8.0_8) ))   STOP 37
        IF ( KIND(U%Cplx) .NE. 8 )                      STOP 38

        IF ( ANY(U%L        .NEQV. .TRUE._8) )   STOP 40
        IF ( KIND(U%L) .NE. 8 )                  STOP 41

        IF ( ANY(U%C    .NE. Str) )   STOP 42
        IF ( LEN(U%C) .NE. 1025 )     STOP 43

      CLASS DEFAULT
        STOP 51
      TYPE IS (Child)
        STOP 44
      END SELECT

    END SELECT

  END SUBROUTINE

  END



