! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  Scope4.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Scope4
!*
!*  DATE                       : Feb. 25, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*  Scope
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Scope4
  IMPLICIT NONE
  CHARACTER(9) :: A(3:6) = "123456789", B(4)="987654321"

  CALL Sub(B)

  IF ( ANY(LBOUND(A) .NE. (/3/) ) )          STOP 31
  IF ( ANY(SHAPE(A)  .NE. (/4/) ) )          STOP 32
  IF ( ANY(A         .NE. "123456789" ) )    STOP 33

  IF ( ANY(LBOUND(B) .NE. (/1/) ) )          STOP 31
  IF ( ANY(SHAPE(B)  .NE. (/4/) ) )          STOP 32
  IF ( ANY(B         .NE. "43219876" ) )     STOP 33

  CONTAINS

  SUBROUTINE Sub(B)
  CLASS(*) :: B(4:7)

  SELECT TYPE (B)
  TYPE IS (CHARACTER(*))

    ASSOCIATE ( A => B )

      IF ( ANY(LBOUND(A) .NE. (/4/) ) )          STOP 11
      IF ( ANY(SHAPE(A)  .NE. (/4/) ) )          STOP 12
      IF ( ANY(A         .NE. "987654321" ) )    STOP 13

      B = "43219876"
      ASSOCIATE ( A => A )
        IF ( ANY(LBOUND(A) .NE. (/4/) ) )        STOP 21
        IF ( ANY(SHAPE(A)  .NE. (/4/) ) )        STOP 22
        IF ( ANY(A         .NE. "43219876" ) )   STOP 23
      END ASSOCIATE
    END ASSOCIATE

  CLASS DEFAULT
    STOP 33
  END SELECT

  IF ( ANY(LBOUND(A) .NE. (/3/) ) )          STOP 31
  IF ( ANY(SHAPE(A)  .NE. (/4/) ) )          STOP 32
  IF ( ANY(A         .NE. "123456789" ) )    STOP 33

  END SUBROUTINE
  END
