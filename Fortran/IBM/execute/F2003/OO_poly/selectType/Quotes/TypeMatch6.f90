! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: TypeMatch6.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : TypeMatch6
!*
!*  DATE                       : Jan. 24, 2005
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
!*  No matching clauses
!*
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
      INTEGER :: Id=1
    END TYPE

    TYPE, EXTENDS(Zero)  :: One
    END TYPE

    TYPE, EXTENDS(One) :: Two
    END TYPE

    TYPE, EXTENDS(Two) :: Three
    END TYPE

  END MODULE

  PROGRAM TypeMatch4
  USE M,  Two=>One , DT=>Two
  IMPLICIT NONE
  CLASS(*), ALLOCATABLE :: U(:,:)

    ALLOCATE(DT :: U(2:3,3:4) )

    SELECT TYPE (One=>U)
    TYPE IS (Zero)
      STOP 40
    TYPE IS (Three)
      STOP 41
    TYpe IS (Two)
      STOP 43
    CLASS IS (Two)
      Print*, "ok!"
    CLASS DEFAULT
      STOP 44
    END SELECT

  END



