! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: C819Labels.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  C819Labels
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
!*    With labels on all statements
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  C819Labels
  IMPLICIT NONE

  TYPE :: Base
  END TYPE

  CLASS(*),   POINTER :: Ptr
  TYPE(Base), TARGET  :: Tar

  Ptr => Tar
111 SELECT :  SELECT TYPE ( Ptr )
1   TYPE IS (Base) SELECT
      goto 11
11    PRINT*, "OK!"
2   CLASS IS (Base)
22    STOP 20
3   CLASS DEFAULT SELECT
33    STOP 30
333 END SELECT  SELECT


  END

