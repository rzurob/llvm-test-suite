! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: InClassDefault3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InClassDefault3
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
!*  check type parameters
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InClassDefault3
  IMPLICIT NONE

  INTEGER(1)      :: Int(2:3,3:4)=8_1
  COMPLEX(8)      :: Cplx(2:3,3:4)=(-8.0_8, 8.0_8)
  LOGICAL(8)      :: L(2:3,3:4)=.true._8
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

    ASSOCIATE ( W => U )

      SELECT TYPE (U => W )

      TYPE IS (INTEGER(1))
        IF ( ANY(U   .NE. 8_1) )      STOP 35
        IF ( KIND(U) .NE. 1 )         STOP 36

      TYPE IS (COMPLEX(8))
        IF ( ANY(U   .NE. (-8.0_8, 8.0_8) ))   STOP 37
        IF ( KIND(U) .NE. 8 )                  STOP 38

      TYPE IS (LOGICAL(8))
        IF ( ANY(U   .NEQV. .TRUE._8) )   STOP 40
        IF ( KIND(U) .NE. 8 )             STOP 41

      TYPE IS (CHARACTER(*))
        IF ( ANY(U  .NE. Str) )   STOP 42
        IF ( LEN(U) .NE. 1025 )   STOP 43

      CLASS DEFAULT
        STOP 51
      END SELECT

    END ASSOCIATE
    END SELECT

  END SUBROUTINE

  END



