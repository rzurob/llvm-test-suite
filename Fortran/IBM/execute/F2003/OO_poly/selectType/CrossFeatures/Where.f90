! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Where.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Where
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
!* Where
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Where
  IMPLICIT CLASS(DT)(U)
  TYPE :: DT
   COMPLEX :: Cplx=(1.0, -1.0)
  END TYPE
  INTEGER :: i

  CALL Sub((/(DT(Cplx=(-1.0,1.0)), i=1,16)/))

  CONTAINS

  SUBROUTINE Sub(U)
  DIMENSION :: U(:)

  SELECT TYPE (U)
  CLASS IS (DT)

    IF (ANY(U%Cplx .NE. (-1.0, 1.0))) STOP 20
    IF (ANY(SHAPE(U).NE. (/16/))) STOP 21

    WHERE ( U%Cplx .EQ. (-1.0,1.0))
      U%Cplx = (1.0, -1.0)
    END WHERE

    IF (ANY(U%Cplx .NE. (1.0, -1.0))) STOP 22

  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END



