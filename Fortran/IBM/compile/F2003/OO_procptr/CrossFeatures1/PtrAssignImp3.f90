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
! %POSTCMD:  tcomp PtrAssignImp3.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignImp3.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 27, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment 
!*
!*  REFERENCE                  : Feature 289058 
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
!*  If proc-pointer-object has an implicit interface and is referenced 
!*  as a subroutine, proc-target shall be a subroutine. 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Child
      INTEGER  :: Id = 2
    END TYPE

  END MODULE

  SUBROUTINE ExtSub(Arg)
  USE M
  TYPE (Child) :: Arg 
    Arg = Child(-2)
  END SUBROUTINE 

  FUNCTION ExtFun()
  USE M
  TYPE (Child) :: ExtFun 
    ExtFun = Child(-1)
  END FUNCTION 

  PROGRAM PtrAssignImp3
  USE M 
  IMPLICIT TYPE(Child)(C) 

  INTERFACE
    SUBROUTINE Extsub(Arg)
      IMPORT Child
      TYPE (Child) :: arg 
    END SUBROUTINE

    FUNCTION ExtFun()
      IMPORT Child
      TYPE (Child) :: ExtFun
    END FUNCTION
  END INTERFACE

  PROCEDURE(),  POINTER :: ProcPtr
  PROCEDURE(),  POINTER :: CProcPtr
  TYPE(Child)           :: V

  ProcPtr => ExtFun 
  CProcPtr => ExtSub

  IF (.TRUE. ) THEN
    PRINT*, CProcPtr(V)
    CALL ProcPtr(Child(-2))
  END IF
 
  END 

