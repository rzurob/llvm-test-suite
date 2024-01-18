! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  Misc17.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:  
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc17 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 04, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!* "1516-078 (S) Operands must be conformable" and incorrect output
!*  for "print*, As%BaseArr(1,1)%BaseId" 
!* (297811 )
!*  
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero
    END TYPE

    TYPE, EXTENDS(Zero) :: Base
      INTEGER :: BaseId = 1
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      TYPE(Base) :: BaseArr(1,1)
    END TYPE

  END MODULE

  PROGRAM Misc17
  USE M
  IMPLICIT NONE
  CLASS(Base), ALLOCATABLE :: Var
  integer i

  ALLOCATE(Child :: Var)

  SELECT TYPE ( As  => RESHAPE( (/(Var,  i=1,4)/), (/2,2/)) )
  TYPE IS (Child)

    print*, SHAPE(As)
    print*, SHAPE(As%BaseArr(1,1)%BaseId)
    print*, As%BaseArr(1,1)%BaseId
    IF ( ANY (SHAPE(As%BaseArr(1,1)%BaseId) .NE. (/2,2/) ) )  STOP 35
    IF ( ANY(As%BaseArr(1,1)%BaseId  .NE. 1) ) STOP 36

  END SELECT

  END

