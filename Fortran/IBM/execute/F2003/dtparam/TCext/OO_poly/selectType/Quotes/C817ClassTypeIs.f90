! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/C817ClassTypeIs.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: C817ClassTypeIs.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C817ClassTypeIs
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C817
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
!*    The same type is specified for both type guard CLASS IS
!*    and TYPE IS
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Level0(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE

    TYPE, EXTENDS(Level0) :: Level1    ! (4)
      INTEGER(K1) :: Level1Id = 1
    END TYPE

    TYPE, EXTENDS(Level1) :: Level2    ! (4)
      INTEGER(K1) :: Level2Id = 2
    END TYPE

    TYPE, EXTENDS(Level2) :: Level3    ! (4)
      INTEGER(K1) :: Level3Id = 3
    END TYPE

    TYPE, EXTENDS(Level3) :: Level4    ! (4)
      INTEGER(K1) :: Level4Id = 4
    END TYPE

  END MODULE

  PROGRAM C817ClassTypeIs
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr
  TYPE(Level2(4)), TARGET  :: Tar

  Ptr  => Tar
  CALL Sub(Ptr)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg

  SELECT TYPE ( Arg )
    TYPE IS (Level1(4))
      STOP 50
    CLASS IS (Level1(4))
      STOP 51
    TYPE IS (Level2(4))
    ! STOP 53
    CLASS IS (Level2(4))
      STOP 55
    CLASS IS (Level4(4))
      STOP 55
    TYPE IS (Level4(4))
      STOP 56
    CLASS DEFAULT
      STOP 30
  END SELECT

  END SUBROUTINE

  END

