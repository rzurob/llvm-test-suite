! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Nullify1.f 
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
!*  TEST CASE NAME             : Nullify1.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 10, 2005
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
!*  The nullify stmt 
!*  in the same NULLIFY statement
!*  (ICE-304576) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Nullify1
  USE M
  IMPLICIT NONE

  TYPE(DT), POINTER       :: V1, V2
  PROCEDURE(Fun), POINTER :: ProcPtr

  TYPE(DT), POINTER       :: W(:)

  ALLOCATE(V1)
  V1 = DT(-1, Fun)
  NULLIFY(V1%ProcPtr)

  !ALLOCATE(V2, SOURCE=DT(-1, Fun)) ! not 10.1
  ALLOCATE(V2)
  V2 = DT(-1, Fun)
! NULLIFY(V1%ProcPtr, V1%ProcPtr)
  NULLIFY(V1%ProcPtr)
  DEALLOCATE(V1)

  !ALLOCATE(W(3), SOURCE=DT(-1, Fun))
  ALLOCATE(W(3))
  W = DT(-1, Fun)
  NULLIFY(W(1)%ProcPtr, W(2)%ProcPtr, W(3)%ProcPtr)
  DEALLOCATE(W)

  END



