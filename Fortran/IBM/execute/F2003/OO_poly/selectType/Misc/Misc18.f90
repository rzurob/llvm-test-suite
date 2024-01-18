! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Misc18.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc18
!*
!*  DATE                       : Jan. 26, 2005
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
!*  Seg Fault - array of multi dimensions
!*  (298758)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc18

  IMPLICIT NONE
  TYPE :: DT
    INTEGER :: Id = 1
  END TYPE

  TYPE(DT),   TARGET   ::    DTV(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)

  CALL Sub()
  CALL Sub(DTV)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*), TARGET, OPTIONAL  :: Arg(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
  END SUBROUTINE

  END


