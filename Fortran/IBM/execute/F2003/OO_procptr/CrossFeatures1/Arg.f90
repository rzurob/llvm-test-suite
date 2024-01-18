! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Arg.f 
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
!*  TEST CASE NAME             : Arg.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 19, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : 
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
!*  Dummy argument is a procedure pointer - procedure pointer/function/Null 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE :: Base
      CHARACTER(3) :: C
    END TYPE
  
  END MODULE

  MODULE M
  USE M0

    TYPE  :: DT
      PROCEDURE(TYPE(Base)), NOPASS, POINTER :: ProcPtr
    END TYPE
   
    CONTAINS
 
    FUNCTION ModFun(Arg)
    TYPE(Base) :: Arg, ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE

  FUNCTION RetPtr(Fun)
  USE M
  PROCEDURE(TYPE(Base))          :: Fun
  PROCEDURE(TYPE(Base)), POINTER :: RetPtr 
    RetPtr => Fun 
  END FUNCTION
 
  PROGRAM Arg  
  USE M
  IMPLICIT TYPE(DT)(P) 
  PROCEDURE(TYPE(Base)), POINTER :: ProcPtr

  INTERFACE
    FUNCTION RetPtr(Fun)
    IMPORT Base
      PROCEDURE(TYPE(Base))          :: Fun
      PROCEDURE(TYPE(Base)), POINTER :: RetPtr
    END FUNCTION
  END INTERFACE

  ProcPtr => ModFun
  CALL IntSub(ProcPtr)

  CONTAINS

  SUBROUTINE IntSub(Ptr)
  PROCEDURE(TYPE(Base)), POINTER :: Ptr
  TYPE(Base) :: V
  TYPE(DT)   :: U, W

  V = Ptr(Base("123"))
  IF ( V%C .NE. "123" ) STOP 12

  U = DT(RetPtr(Ptr))
  IF ( .NOT. ASSOCIATED(U%ProcPtr, Ptr) ) STOP 32

  END SUBROUTINE 

  END

