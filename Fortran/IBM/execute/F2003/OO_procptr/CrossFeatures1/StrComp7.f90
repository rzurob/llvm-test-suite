! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: StrComp7.f 
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
!*  TEST CASE NAME             : StrComp7.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 18, 2005
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
!*  Procedure pointer components 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      CHARACTER(3) :: C
     PROCEDURE(CHARACTER(3)), NOPASS, POINTER :: ProcPtr
    END TYPE

    TYPE  :: DT
      TYPE(Base) :: BComp
      PROCEDURE(CHARACTER(3)), NOPASS, POINTER :: ProcPtr
    END TYPE
   
    CONTAINS
 
    FUNCTION ModFun(Arg)
    CHARACTER(3) :: Arg, ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE
 
  PROGRAM StrComp7  
  USE M, B=>Base, D=>DT, IntF=>ModFun 
  IMPLICIT CHARACTER(3)(P) 

  PROCEDURE(IntF),  POINTER :: ProcPtr=>NULL() 
  TYPE(D)  :: U

  ProcPtr => IntF
  IF ( ProcPtr("123") .NE. "123" ) STOP 11

  U = D(B("123", IntF), ProcPtr)
  IF ( U%BComp%C .NE. "123" )              STOP 21
  IF ( .NOT. ASSOCIATED(U%ProcPtr, IntF))  STOP 22
  IF ( U%ProcPtr("!!!") .NE. "!!!" )       STOP 24
  IF ( U%BComp%ProcPtr("!!!") .NE. "!!!" ) STOP 25


  END

