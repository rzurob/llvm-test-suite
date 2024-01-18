! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: IO1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : IO1
!*
!*  DATE                       : Feb. 02, 2005
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
!* IO
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM IO1
  IMPLICIT CLASS(DT)(U)
  IMPLICIT CLASS(*)(V)
  TYPE :: DT
    INTEGER :: Int
    CHARACTER(63) :: C
  END TYPE
  INTEGER :: i
  CHARACTER(3000) :: C="321", a

  CALL Sub((/(DT(Int=6, C="123"), i=1,16)/), C)

  CONTAINS

  SUBROUTINE Sub(U, V)
  DIMENSION :: U(:)
  INTENT(INOUT)  :: V

  SELECT TYPE (U)
  CLASS IS (DT)

    IF (ANY(U%Int   .NE. 6))       STOP 20
    IF (ANY(U%C     .NE. "123"))   STOP 21
    IF (ANY(SHAPE(U).NE. (/16/)))  STOP 22

    WRITE(U(1)%C, FMT=*) U(SIZE(U))%C(1:LEN(U(1)%C)-1)
    IF (INDEX(U(1)%C, "123") .EQ. 0 ) STOP 20

  CLASS DEFAULT
    STOP 40
  END SELECT

  SELECT TYPE (V)
  CLASS DEFAULT
    STOP 40
  TYPE IS (CHARACTER(*))
    WRITE(V(4:), FMT=*) V(1:3)
    IF (INDEX(V, "321") .EQ. 0 ) STOP 30
  END SELECT

  END SUBROUTINE

  END



