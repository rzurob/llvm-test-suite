! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Associated4.f 
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
!*  TEST CASE NAME             : Associated4.f 
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
!*  ASSOCIATED(POINTER [, TARGET]) 
!*  TARGET is a procedure pointer 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
   
    TYPE :: DT
      INTEGER :: Id=0
    END TYPE

    CONTAINS

    FUNCTION FProcPtr(Arg)
    PROCEDURE(TYPE(DT)), POINTER :: FPRocPtr 
    PROCEDURE(TYPE(DT))          :: Arg 
      FProcPtr => Arg
    END FUNCTION 

    FUNCTION FDT()
    TYPE(DT) :: FDT
      FDT = DT(-1)
    END FUNCTION

  END MODULE

  
  PROGRAM Associated4 
  USE M
  IMPLICIT NONE 
  PROCEDURE(TYPE(DT)), POINTER :: ProcPtr=>NULL() 
  PROCEDURE(TYPE(DT)), POINTER :: ProcPtr1=>NULL() 


  IF ( ASSOCIATED( ProcPtr, NULL(ProcPtr)))   STOP 11

  ProcPtr => FDT 
  IF ( .NOT. ASSOCIATED( ProcPtr, ProcPtr))   STOP 12
 
  IF ( .NOT. ASSOCIATED( ProcPtr, FProcPtr(ProcPtr)))  STOP 13
  IF ( .NOT. ASSOCIATED( ProcPtr, FProcPtr(FDT)))      STOP 14
  
  ProcPtr => ProcPtr
  PRINT*, ProcPtr()

  IF ( ASSOCIATED( ProcPtr, ProcPtr1))  STOP 15
  IF ( ASSOCIATED( ProcPtr1, ProcPtr))  STOP 16
  
 END

