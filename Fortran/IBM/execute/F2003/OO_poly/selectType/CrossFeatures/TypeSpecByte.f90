! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: TypeSpecByte.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : TypeSpecByte
!*
!*  DATE                       : Jan. 28, 2005
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
!*  Type Spec : Byte
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  TypeSpecByte
  IMPLICIT NONE

  BYTE :: R1(2,2,2) = -1

  CALL Sub(R1)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*)  :: Arg(:,:,:)


S1: SELECT TYPE (S2 => Arg(:,:,:))
    CLASS DEFAULT

S2: SELECT TYPE (U => S2 )
    CLASS DEFAULT
      STOP 20
    TYPE IS (BYTE)

        IF (SIZE(U)       .NE. 8)            STOP 30
        IF (ANY(SHAPE(U)  .NE. (/2,2,2/) ))  STOP 31
        IF (KIND(U)       .NE. 1)            STOP 32
        IF (ANY(U         .NE. -1))          STOP 33

        U = -2

        IF (ANY(U    .NE. -2))   STOP 34

    END SELECT S2
    END SELECT S1

  END SUBROUTINE

  END



