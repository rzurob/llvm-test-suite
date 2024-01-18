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
! %POSTCMD: tcomp C819NotSame.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C819NotSame
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C819
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
!*    The select type construct name is not consistent.
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM C819NotSame
  IMPLICIT NONE

  TYPE :: Base
  END TYPE

  CLASS(*),   POINTER :: Ptr
  TYPE(Base), TARGET  :: Tar

  Ptr => Tar

0 SELECT : SELECT TYPE ( Ptr )
1   TYPE IS (Base) SELECT
11    PRINT*, "OK!"
2   CLASS IS (Base) END
22    STOP 20
3   CLASS DEFAULT
33    STOP 30
4 END SELECT END


  END

