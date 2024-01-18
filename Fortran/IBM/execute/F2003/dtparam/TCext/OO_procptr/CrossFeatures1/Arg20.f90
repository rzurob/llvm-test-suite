! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures1/Arg20.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qck -qnok

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Arg20.f 
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
!*  TEST CASE NAME             : Arg20.f 
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
!*  Argument association - Implicit interface
!*  If the dummy argument is referenced as a subroutine, the actual argumenti
!*  shall be  a subroutine, subroutine procedure pointer, or dummy procedure.
!* (304228)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  IMPLICIT TYPE(Base(4,3))(P)

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C
    END TYPE
 
    INTERFACE
      SUBROUTINE IntF(Arg1, Arg2)
      IMPORT
        TYPE(Base(4,*)), INTENT(IN)  :: Arg2 
        TYPE(Base(4,*)), INTENT(OUT) :: Arg1 
      END SUBROUTINE 
    END INTERFACE

 
  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(Base(4,*)), INTENT(IN)  :: Arg2 
  TYPE(Base(4,*)), INTENT(OUT) :: Arg1 
    Arg1 = Arg2
  END SUBROUTINE 


  PROGRAM Arg20
  USE M
  IMPLICIT TYPE(Base(4,3))(P)
  PROCEDURE(IntF) :: ExtSub

  CALL IntSub( IntFun1(ExtSub), IntFun1(ExtSub))
  CALL IntSub( IntFun1(ExtSub), IntFun2(ExtSub))


  CONTAINS

  FUNCTION IntFun1(Proc)
  PROCEDURE(IntF) :: Proc
  PROCEDURE(IntF), POINTER :: IntFun1
    IntFun1 => Proc
  END FUNCTION

  FUNCTION IntFun2(Proc)
  PROCEDURE() :: Proc
  PROCEDURE(), POINTER :: IntFun2
    IntFun2 => Proc
  END FUNCTION


  SUBROUTINE IntSub(ProcPtr0, ProcPtr1)
  IMPLICIT TYPE(Base(4,3))(P)

  PROCEDURE(IntF),       POINTER :: ProcPtr0
  PROCEDURE(IntF),       POINTER :: ProcPtr1
  TYPE(Base(4,3))                     :: V

  IF ( .NOT. ASSOCIATED(ProcPtr0, ExtSub) ) STOP 10
  IF ( .NOT. ASSOCIATED(ProcPtr1, ExtSub) ) STOP 11

  CALL ProcPtr0(V, Base(4,3)("321"))
  IF (V%C .NE. "321") STOP 15

  CALL ProcPtr1(V, Base(4,3)("123"))
  IF (V%C .NE. "123") STOP 13

  END SUBROUTINE

  END

