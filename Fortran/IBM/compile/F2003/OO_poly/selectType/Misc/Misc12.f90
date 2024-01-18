! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp Misc12.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc11
!*
!*  DATE                       : Jan. 05, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*  ICE -  inaccessible type bound procedure
!*    (297671)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero
    CONTAINS
      PROCEDURE, NOPASS   :: ReturnObj
    END TYPE

    CONTAINS

    FUNCTION ReturnObj1(Arg)
    CLASS(Zero) :: Arg
    CLASS(*), ALLOCATABLE :: ReturnObj1
      ALLOCATE(ReturnObj1, SOURCE=Arg)
    END FUNCTION

  END MODULE


  PROGRAM  Misc12

  END

