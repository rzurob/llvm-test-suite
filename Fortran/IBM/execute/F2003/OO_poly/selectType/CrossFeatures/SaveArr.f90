! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SaveArr.f 
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
!*  TEST CASE NAME             : SaveArr
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
      INTEGER(4)      :: IArr(2)=1
      CHARACTER(1025) :: CArr(2)="!"
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
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

  PROGRAM SaveArr
  USE M
  IMPLICIT NONE
  TYPE(DT), TARGET :: V(2,2)

  V = Fun() 
    IF (ANY(V%IArr(1) .NE. 1)) STOP 21
    IF (TRIM(V(1,1)%CArr(1)) .NE. "!") STOP 22
    IF (TRIM(V(2,2)%CArr(2)) .NE. "!") STOP 23

  CONTAINS

  RECURSIVE FUNCTION Fun()
  CLASS(DT), SAVE, POINTER :: U(:,:)
  INTEGER :: i
  CLASS(DT), POINTER :: Fun(:,:)

    ALLOCATE(U(2,2), SOURCE=DT(IArr=1))

  SELECT TYPE( U  )
  CLASS IS (DT)

    IF (ANY(U%IArr(1) .NE. 1)) STOP 31
    IF (TRIM(U(1,1)%CArr(1)) .NE. "!") STOP 32
    IF (TRIM(U(2,2)%CArr(2)) .NE. "!") STOP 33

    Fun => U

  CLASS DEFAULT
    STOP 40
  END SELECT

  END FUNCTION 

  END



