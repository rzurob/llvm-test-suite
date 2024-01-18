! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Arg5.f 
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
!*  TEST CASE NAME             : Arg4.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 20, 2005
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
!*  Dummy procedure -  
!*                     a function returning a procedure pointer
!*  (ICE) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE :: Base
      SEQUENCE
      PROCEDURE(), POINTER, NOPASS :: ProcPtr
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base) :: Arg, IntF
      END FUNCTION
    END INTERFACE

    LOGICAL :: OK=.FALSE.

    CONTAINS

    SUBROUTINE ModSub()
      OK = .TRUE.
      print*, "ok"
    END SUBROUTINE

    FUNCTION RetPtr(Arg)
    PROCEDURE(IntF) :: Arg
    PROCEDURE(IntF), POINTER :: RetPtr
      RetPtr => Arg
    END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base) :: Arg, ExtFun
    ExtFun = Arg
  END FUNCTION

  PROGRAM Arg5
  USE M
  IMPLICIT NONE
  PROCEDURE(IntF)          :: ExtFun
  PROCEDURE(IntF), POINTER :: ProcPtr
  TYPE(Base)               :: V

  ProcPtr => RetPtr(ExtFun)
  V = ProcPtr(Base(ModSub))
  IF (.NOT. ASSOCIATED(V%ProcPtr, ModSub) ) STOP 11
  CALL V%ProcPtr()
  IF ( .NOT. OK )  STOP 12

  END


