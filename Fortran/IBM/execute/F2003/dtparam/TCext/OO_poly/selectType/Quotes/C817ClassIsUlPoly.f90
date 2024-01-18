! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/selectType/Quotes/C817ClassIsUlPoly.f
! opt variations: -qnok -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: C817ClassIsUlPoly.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C817ClassIsUlPoly
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
!*    The type guard CLASS IS is specified with the same type twice
!*    The selector is unlimited poly
!*    (E-level, compilation successful)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Level0(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Level0) :: Level1    ! (4,20)
      INTEGER(K1) :: Level1Id = 1
    END TYPE

    TYPE, EXTENDS(Level1) :: Level2    ! (4,20)
      INTEGER(K1) :: Level2Id = 2
    END TYPE

    TYPE, EXTENDS(Level2) :: Level3    ! (4,20)
      INTEGER(K1) :: Level3Id = 3
    END TYPE

    TYPE, EXTENDS(Level3) :: Level4    ! (4,20)
      INTEGER(K1) :: Level4Id = 4
    END TYPE

  END MODULE

  PROGRAM C817ClassIsPoly
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr
  INTEGER, TARGET  :: Tar

  Ptr  => Tar

  SELECT TYPE ( Ptr )
    TYPE IS (Level4(4,*))
      STOP 50
    CLASS IS (Level4(4,*))
      STOP 51
    CLASS IS (Level1(4,*))
      STOP 53
    CLASS IS (Level4(4,*))
      STOP 55
    CLASS IS (Level1(4,*))
      STOP 55

    CLASS DEFAULT
    ! STOP 30
  END SELECT

  SELECT TYPE ( Ptr )
    TYPE IS (INTEGER)
    ! STOP 42
      PRINT*, "Still going here!"
    TYPE IS (INTEGER(4))
      STOP 43
  END SELECT

  SELECT TYPE ( Ptr )
    TYPE IS (REAL(8))
      STOP 32
    TYPE IS (DOUBLE PRECISION)
      STOP 33
  END SELECT

  SELECT TYPE ( Ptr )
    TYPE IS (LOGICAL)
      STOP 62
    TYPE IS (LOGICAL(4))
      STOP 63
  END SELECT

  END

