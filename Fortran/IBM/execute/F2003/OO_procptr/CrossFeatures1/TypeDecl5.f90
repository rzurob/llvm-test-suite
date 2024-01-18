! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: TypeDecl5.f  
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
!*  TEST CASE NAME             :  TypeDecl5.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 28, 2005
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
!*  Usage of BIND(C) 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  USE ISO_C_BINDING

  INTERFACE
    FUNCTION CF() BIND(C)
    IMPORT
      INTEGER(C_INT) :: CF
    END FUNCTION
  END INTERFACE

  INTERFACE
    SUBROUTINE CS(Arg) BIND(C)
    IMPORT
      INTEGER(C_INT) :: Arg 
    END SUBROUTINE 
  END INTERFACE 

  PROCEDURE(CF), POINTER, BIND(C) :: ProcPtr

 
  END MODULE

  FUNCTION ExtFun() BIND(C)
  USE ISO_C_BINDING 
  INTEGER(C_INT) :: ExtFun 
    ExtFun = -1_C_INT
  END FUNCTION

  SUBROUTINE ExtSub(Arg) BIND(C)
  USE ISO_C_BINDING
    INTEGER(C_INT) :: Arg
    Arg = 1_C_INT
  END SUBROUTINE

  PROGRAM TypeDecl5 
  USE M

  !PROCEDURE(CF), BIND(C, NAME="extfun") :: ExtFun 
  PROCEDURE(CF), BIND(C) :: ExtFun 
  PROCEDURE(CS), BIND(C) :: ExtSub

  PROCEDURE(CF), BIND(C), POINTER :: PExtFun 
  PROCEDURE(CS), BIND(C), POINTER :: PExtSub

  INTEGER(C_INT) :: I

  IF(  ExtFun() .NE. -1_C_INT ) STOP 11

  CALL ExtSub(I) 
  IF( I .NE. 1_C_INT )  STOP 12

  PExtFun => ExtFun
  IF(  PExtFun() .NE. -1_C_INT ) STOP 13
 
  PExtSub => ExtSub
  CALL PExtSub(I)
  IF( I .NE. 1_C_INT )  STOP 14

  END


