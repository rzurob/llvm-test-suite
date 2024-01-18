! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SaveVar.f 
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
!*  TEST CASE NAME             : SaveVar
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 02, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     
!*  Save 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0
      INTEGER(4)      :: IArr(2)=5
      CHARACTER(1025) :: CArr(2)="5"
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
      CLASS(DT0), POINTER, PRIVATE :: Ptr
      CONTAINS
      PROCEDURE, NoPASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      PRIVATE
    END TYPE

    CONTAINS

    FUNCTION GetObj(Arg)
    CLASS(*),TARGET, INTENT(IN) :: Arg
    CLASS(*), POINTER  :: GetObj
      GetObj => Arg
    END FUNCTION

  END MODULE

  PROGRAM SaveVar 
  USE M
  IMPLICIT NONE
  TYPE(DT), TARGET :: V(2,2)

  CALL Sub(5)

  CONTAINS

  RECURSIVE SUBROUTINE Sub(I)
  CLASS(DT), SAVE, POINTER :: U(:,:)
  INTEGER :: i

  IF(i .EQ. 5) THEN
    ALLOCATE(U(2,2))
  END IF 
  SELECT TYPE( U )
  CLASS IS (DT)

    IF (ANY(U%IArr(1) .NE. i)) STOP 21
    IF (TRIM(U(1,1)%CArr(1)) .NE. CHAR(ICHAR("0")+i)) STOP 22
    IF (TRIM(U(2,2)%CArr(2)) .NE. CHAR(ICHAR("0")+i)) STOP 23

    U%IArr(1) = i-1 
    U%IArr(2) = i-1 
    U%CArr(1) = CHAR(ICHAR("0")+i-1) 
    U%CArr(2) = CHAR(ICHAR("0")+i-1)

    IF (I .GT. 1) THEN
      CALL Sub(i-1)
    END IF     
  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END



