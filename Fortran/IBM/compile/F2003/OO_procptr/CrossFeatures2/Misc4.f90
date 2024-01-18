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
! %POSTCMD: tcomp Misc4.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc4.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 26, 2005
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
!*  Procedure pointer - Diag with type binding 
!*  (315064)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT
      PROCEDURE(ModFun), PASS, POINTER :: ProcPtr
      CONTAINS
      PROCEDURE, PASS :: Proc => ModFun 
    END TYPE

  CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT) :: Arg
    TYPE(DT)  :: ModFun
      ModFun = Arg 
    END FUNCTION

  END MODULE

  PROGRAM Misc4 
  USE M

  TYPE(DT) :: V
  PROCEDURE(ModFun), POINTER :: ProcPtr

  ! This is ok
  TYPE :: DT1
    PROCEDURE(ModFun), NOPASS, POINTER :: ProcPtr
  END TYPE
  TYPE(DT1) :: U

  ! All the following are wrong 

! no need to test it as it will show sytax errors
! TYPE :: DT2
!   PROCEDURE(V%ProcPtr), NOPASS, POINTER :: ProcPtr1
!   PROCEDURE(V%Proc),    NOPASS, POINTER :: ProcPtr2
! END TYPE

! PROCEDURE(V%ProcPtr), POINTER :: ProcPtr3
! PROCEDURE(V%Proc),    POINTER :: ProcPtr4

  ProcPtr => V%Proc

  ProcPtr => U%ProcPtr  ! this is ok

  END

