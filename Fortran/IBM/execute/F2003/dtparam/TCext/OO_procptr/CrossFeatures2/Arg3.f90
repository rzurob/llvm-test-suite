! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_procptr/CrossFeatures2/Arg3.f
! opt variations: -qck -qnok -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Arg3.f 
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
!*  TEST CASE NAME             : Arg3.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 27, 2005
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
!*  If an external procedure name or a dummy procedure name is used as an actual
!*  argument, its interface shall be explicit or it shall be explicitly
!*  declared to have the EXTERNAL attribute
!* 
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M0

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C
    END TYPE

  END MODULE

  MODULE M
  USE M0

    TYPE, EXTENDS(Base)  :: DT(K2,N2)    ! (4,3,4,20)
        INTEGER, KIND :: K2
        INTEGER, LEN  :: N2
      PROCEDURE(IFun), PASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, PASS :: Proc=>ModFun
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT(4,*,4,*)) :: Arg
    CLASS(DT(4,:,4,:)), POINTER ::  ModFun
      ALLOCATE(ModFun, SOURCE=Arg)
    END FUNCTION

    FUNCTION IFun(Arg)
    CLASS(DT(4,*,4,*)) :: Arg
    CLASS(DT(4,:,4,:)), POINTER ::  IFun
      ALLOCATE(IFun, SOURCE=Arg)
    END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  CLASS(DT(4,*,4,*)) :: Arg
  CLASS(DT(4,:,4,:)), POINTER ::  ExtFun
    ALLOCATE(ExtFun, SOURCE=Arg) 
  END FUNCTION

  PROGRAM Arg3 
  USE M
  IMPLICIT NONE 
  PROCEDURE(IFun) :: ExtFun
  PROCEDURE(ExtFun), POINTER :: ProcPtr 

  CALL IntSub1(ExtFun )

  ProcPtr => ExtFun
  CALL IntSub1( ProcPtr )

  CONTAINS

  SUBROUTINE IntSub1(Arg)
  PROCEDURE(IFun) :: Arg
  TYPE(DT(4,3,4,20)) :: V, U

    V = Arg(DT(4,3,4,20)("123", Arg))
    IF (V%C .NE. "123")                   STOP 11
    IF (.NOT. ASSOCIATED(V%ProcPtr, Arg)) STOP 12

    V%C = "321"
    V%ProcPtr => IFun 
    U = V%Proc()
    IF (U%C .NE. "321")                    STOP 21
    IF (.NOT. ASSOCIATED(U%ProcPtr, IFun)) STOP 22

  END SUBROUTINE

  END

