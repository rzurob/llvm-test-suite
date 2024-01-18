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
! %POSTCMD: tcomp PtrAssignC7251.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignC7251.f
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
!* C725 (R741) the procedure-component-name shall be the name of a
!* procedure pointer component of the declared type of variable.
!* 
!* 
!*  (304382) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      CHARACTER(1),       POINTER          :: CharPtr => NULL()
      PROCEDURE (ModFun), POINTER, NOPASS  :: ProcPtr => NULL()
    END TYPE

  TYPE(DT), SAVE :: MV

  CONTAINS 
    FUNCTION ModFun(Arg)
    CHARACTER(1) :: ModFun, Arg
      ModFun = Arg
    END FUNCTION
  END MODULE


  PROGRAM  PtrAssignC7251
  USE M
  IMPLICIT NONE

  TYPE :: PDT
    TYPE(DT) :: Base
    PROCEDURE (CHARACTER(1)), NOPASS, POINTER :: ProcPtr
  END TYPE

  TYPE(PDT) :: LV

  PROCEDURE(CHARACTER(1)) :: ExtFun 


    MV%CharPtr => ModFun
    MV%CharPtr => ExtFun
    MV%ProcPtr => ExtFun
 
    LV%Base%CharPtr => ModFun
    LV%Base%CharPtr => ExtFun
    LV%Base%ProcPtr => MV%CharPtr 
 
    LV%ProcPtr => ModFun
    LV%ProcPtr => MV%CharPtr 
    LV%ProcPtr => MV%ProcPtr 
 
    LV%Base%CharPtr => ModFun

  END

  
  FUNCTION ExtFun(Arg)
  CHARACTER(1) :: ExtFun, Arg
    ExtFun = Arg 
  END FUNCTION

