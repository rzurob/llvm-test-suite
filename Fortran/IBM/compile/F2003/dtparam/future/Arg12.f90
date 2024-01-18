! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures1/Arg12.f
! opt variations: -qck -qnok

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
! %POSTCMD: tcomp Arg12.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg12.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 23, 2005
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
!*  Explicit dummy procedure - Characteristics 
!*  Implicit/Explicit Interface 
!*  (304109) - Defered to feature 304991
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C
    END TYPE
 
    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base(4,*)) :: Arg 
        TYPE(Base(4,3)):: IntF 
      END FUNCTION
    END INTERFACE
 
  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(4,*)) :: Arg 
  TYPE(Base(4,3)) :: ExtFun
    ExtFun = Arg
  END FUNCTION


  PROGRAM Arg12
  USE M
  IMPLICIT NONE

  PROCEDURE(IntF) :: ExtFun 
  PROCEDURE(IntF), POINTER :: ProcPtr1 
  PROCEDURE(TYPE(Base(4,3))), POINTER :: ProcPtr2 

  ProcPtr1 => ExtFun
  CALL IntSub(ProcPtr1)

  ProcPtr2 => ExtFun
  CALL IntSub1(ProcPtr2)  !This is wrong


  CONTAINS

    SUBROUTINE IntSub(Arg)
    IMPLICIT NONE
    PROCEDURE(TYPE(Base(4,3))), POINTER :: Arg
    END SUBROUTINE

    SUBROUTINE IntSub1(Arg)
    IMPLICIT  NONE
    PROCEDURE(IntF), POINTER :: Arg
    END SUBROUTINE


  END

