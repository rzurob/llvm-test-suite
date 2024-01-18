! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Associated.f 
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
!*  TEST CASE NAME             : Associated.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 28, 2005
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
!* 
!*  (314850/315295) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    LOGICAL :: LSub = .FALSE.

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(*)          :: Arg
    CLASS(*), ALLOCATABLE :: ModFun 
      ALLOCATE(ModFun, SOURCE=Arg) 
    END FUNCTION

    SUBROUTINE ModSub(Arg)
    CLASS(*) :: Arg
      LSub = .TRUE. 
    END SUBROUTINE 

  END MODULE

  FUNCTION ExtFun(Arg)
  CLASS(*), ALLOCATABLE :: ExtFun
  CLASS(*)              :: Arg
    ALLOCATE(ExtFun, SOURCE=Arg) 
  END FUNCTION

  
  PROGRAM Associated0 
  USE M
  IMPLICIT NONE 
  PROCEDURE(ModFun), POINTER :: ProcPtr
 

  ProcPtr => ModFun 
  CALL IntSub(ModSub, ProcPtr )

  CONTAINS

  SUBROUTINE  IntSub(Proc, ProcPtr)
  PROCEDURE(ModSub)          :: Proc
  PROCEDURE(ModFun), POINTER :: ProcPtr 

  INTERFACE ExtFun 
    FUNCTION ExtFun(Arg)
      CLASS(*), ALLOCATABLE :: ExtFun
      CLASS(*)              :: Arg
    END FUNCTION
  END INTERFACE


  PROCEDURE(ModFun),  POINTER :: ProcPtr1 => NULL()
  PROCEDURE(ExtFun),  POINTER :: ProcPtr2 => NULL()
  PROCEDURE(ModSub),  POINTER :: ProcPtr3 => NULL()


  IF ( ASSOCIATED( ProcPtr1 ))              STOP 11
  ProcPtr1 => ProcPtr 
  IF ( .NOT. ASSOCIATED(ProcPtr1, ModFun )) STOP 12
  SELECT TYPE (As => ProcPtr1(100_1) )
  TYPE IS (INTEGER(1))
    IF ( As .NE. 100_1 )                    STOP 13
  CLASS DEFAULT
    STOP 14
  END SELECT

  IF ( ASSOCIATED( ProcPtr2 ))              STOP 21
  ProcPtr2 => ExtFun 
  IF ( .NOT. ASSOCIATED(ProcPtr2, ExtFun )) STOP 22
  SELECT TYPE (As => ProcPtr2((1.,-1.)) )
  TYPE IS (COMPLEX)
    IF ( As .NE. (1.0,-1.0) )               STOP 23
  CLASS DEFAULT
    STOP 24
  END SELECT

  IF ( ASSOCIATED( ProcPtr3 ))              STOP 31
  ProcPtr3 => Proc 
  IF ( .NOT. ASSOCIATED(ProcPtr3, ModSub )) STOP 32
  CALL ProcPtr3("12345")
  IF (  .NOT. LSub )                        STOP 33


  END SUBROUTINE

  END

