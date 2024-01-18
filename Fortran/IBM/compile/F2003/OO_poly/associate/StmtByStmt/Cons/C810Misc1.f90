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
! %POSTCMD: tcomp C810Misc1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C810Misc1
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     C810 : Miscellaneous problems found
!*     - IMPLICIT does not work within select construct
!*    (Pass exec) :" V = 2" is not correct usage
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C810Misc1
  IMPLICIT NONE

  TYPE :: DT
    INTEGER :: i = 1
  END TYPE

  CLASS(DT), ALLOCATABLE :: T

  ALLOCATE(T)

  SELECT TYPE ( T )
     TYPE IS (DT)
       v= 2
       print*, v
  END SELECT

  END

