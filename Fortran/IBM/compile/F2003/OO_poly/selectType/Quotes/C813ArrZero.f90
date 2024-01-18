! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp C813ArrZero.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C813ArrZero
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C813
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
!*    The selector is an associating entity of poly array of zero size
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Ground
    END TYPE

    TYPE, EXTENDS(Ground) :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    END TYPE

  END MODULE

  PROGRAM C813ArrZero
  USE M
  IMPLICIT NONE
  CLASS(*), ALLOCATABLE :: Arr(:)

  ALLOCATE( Base :: Arr(4) )

  SELECT TYPE ( As => Arr(1:0) )
  TYPE IS (Base)
    SELECT TYPE ( As )
      TYPE IS (Child)
        STOP 51
      CLASS DEFAULT
        STOP 31
    END SELECT
  END SELECT

  END

