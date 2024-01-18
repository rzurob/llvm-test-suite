! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: InClassIs2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InClassIs2
!*
!*  DATE                       : Jan. 21, 2005
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
!*  Within the CLASS IS, the associating entity is polymorphic
!*  the associating entity assumes the selector's type parameters
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
      INTEGER(1) :: Int=8_1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      COMPLEX(8) :: Cplx=(-8.0_8, 8.0_8)
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      LOGICAL(8) :: L=.true._8
      CHARACTER(1025) :: C
    END TYPE

  END MODULE


  PROGRAM InClassIs2
  USE M
  IMPLICIT NONE
  TYPE(Child) :: V(2:3,3:4)
  CHARACTER(1025) :: Str

  Str(:) = '!'
  V%C(:) = Str
  CALL Sub(V(2:3,3:4))

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(2:3,3:4)
  INTEGER :: i

    SELECT TYPE (U => Arg)
    CLASS IS (Base)
       STOP 43
    CLASS IS (Child)
      SELECT TYPE (U)
      CLASS IS (Child)

        IF ( .NOT. SAME_TYPE_AS(U, Arg))       STOP 30
        IF ( SIZE(U)          .NE. 4 )          STOP 31
        IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) STOP 32
        IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) STOP 33
        IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   STOP 34

        IF ( ANY(U%Int        .NE. 8_1) )      STOP 35
        IF ( KIND(U%Int) .NE. 1 )              STOP 35

        IF ( ANY(U%Cplx       .NE. (-8.0_8, 8.0_8) ))   STOP 35
        IF ( KIND(U%Cplx) .NE. 8 )                      STOP 35

        IF ( ANY(U%L        .NEQV. .TRUE._8) )   STOP 35
        IF ( KIND(U%L) .NE. 8 )                  STOP 35

        IF ( ANY(U%C    .NE. Str) )   STOP 35
        IF ( LEN(U%C) .NE. 1025 )     STOP 35

      CLASS DEFAULT
         STOP 51
      END SELECT

    TYPE IS (Base)
       STOP 40
    CLASS DEFAULT
       STOP 41
    END SELECT

  END SUBROUTINE

  END



