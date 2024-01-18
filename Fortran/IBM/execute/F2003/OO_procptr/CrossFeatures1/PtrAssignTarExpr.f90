! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: PtrAssignTarExpr.f 
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
!*  TEST CASE NAME             : PrtAssignTarExpr.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 12, 2005
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
!*  C726 (R742) An expr shall be a reference to a function whose result
!*  is a procedure pointer.
!* 
!* 
!*  (304388) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION FunI(Arg)
        CHARACTER(3) :: FunI, Arg
      END FUNCTION
    END INTERFACE

    !ABSTRACT INTERFACE 
    INTERFACE 
      FUNCTION FunRetPtr1(Arg)
        IMPORT
        PROCEDURE(FunI), POINTER :: FunRetPtr1
        PROCEDURE(FunI)          :: Arg
      END FUNCTION
    END INTERFACE
    
    INTERFACE 
      FUNCTION FunRetPtr2(Arg)
        IMPORT
        PROCEDURE(FunI), POINTER :: FunRetPtr2, Arg
      END FUNCTION
    END INTERFACE
    
  CONTAINS 

    FUNCTION ModFun(Arg)
    PROCEDURE(FunI), POINTER :: ModFun
    PROCEDURE(FunI)          :: Arg
      PRINT *, "I like this!"
      ModFun => Arg
    END FUNCTION

    FUNCTION ModFun1(Arg)
    PROCEDURE(FunI), POINTER :: ModFun1, Arg
      PRINT *, "I like this too!"
      ModFun1 => Arg
    END FUNCTION

    FUNCTION FChar(Arg)
    CHARACTER(3) :: FChar, Arg
      FChar = Arg
    END FUNCTION
  END MODULE


  PROGRAM PrtAssignTarExpr 
  USE M
  IMPLICIT NONE

  PROCEDURE(FunRetPtr1), POINTER :: Ptr1 
  PROCEDURE(FunRetPtr2), POINTER :: Ptr2 
  PROCEDURE(FunI),       POINTER :: Ptr
 
    Ptr1  => ModFun
    Ptr   => Ptr1(FChar) 
    IF (Ptr("abc") .NE. "abc" ) STOP 11

    Ptr2  => ModFun1
    Ptr   => Ptr2(Ptr1(FChar)) 
    IF (Ptr("cba") .NE. "cba" ) STOP 12

  END

