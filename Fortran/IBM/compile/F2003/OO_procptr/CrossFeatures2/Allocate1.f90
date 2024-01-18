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
! %POSTCMD: tcomp Allocate1.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Allocate1.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 9, 2005
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
!*  The allocate stmt 
!*   
!*  (ICE) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(INTEGER),NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    CONTAINS
  
    FUNCTION ModFun()
    TYPE(DT) :: ModFun
      ModFun = DT(-1, NULL())
    END FUNCTION
 
  END MODULE

  PROGRAM Allocate1 
  USE M
  IMPLICIT NONE 
  
  PROCEDURE(TYPE(DT)), POINTER :: ProcPtr=>NULL() 
  TYPE ( DT ),         POINTER :: V
  PROCEDURE(INTEGER)           :: Fun


  ALLOCATE(ProcPtr)
  ALLOCATE(ProcPtr, SOURCE=Fun)
  ALLOCATE(ProcPtr, SOURCE=DT())
  ALLOCATE(ProcPtr, SOURCE=Fun(0))

  ProcPtr => ModFun
  DEALLOCATE(ProcPtr)

  ALLOCATE(V%ProcPtr)
  ALLOCATE(V%ProcPtr, SOURCE=DT(Fun(0), NULL()))
  ALLOCATE(V%ProcPtr, SOURCE=ProcPtr)
  ALLOCATE(V%ProcPtr, SOURCE=DT())
  ALLOCATE(V%ProcPtr, SOURCE=Fun(0))

  V%ProcPtr => Fun
  DEALLOCATE(V%ProcPtr)

  END

  FUNCTION Fun(Arg)
  INTEGER :: Fun
  INTEGER :: Arg
    Fun = Arg
  END FUNCTION
 
