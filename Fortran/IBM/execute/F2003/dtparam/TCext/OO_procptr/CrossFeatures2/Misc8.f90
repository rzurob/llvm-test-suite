! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_procptr/CrossFeatures2/Misc8.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Misc8.f 
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
!*  TEST CASE NAME             : Misc8.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 30, 2005
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
!*  Procedure pointer - usage of interface 
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  END TYPE

  END MODULE

  PROGRAM Misc7 
  USE M
  IMPLICIT NONE 

  PROCEDURE(IntSub), POINTER :: ProcPtr1
  PROCEDURE(IntFun), POINTER :: PRocPtr2

  PROCEDURE(IntSub) :: ExtSub
  PROCEDURE(IntFun) :: ExtFun 

  TYPE(DT(20,4)) :: V(10000), U(10000)
  INTEGER  :: I


  ProcPtr1 => ExtSub 
  ProcPtr2 => ExtFun

  U = (/(DT(20,4)(-I), I=1, 10000)/) 
  V = (/(DT(20,4)(I), I=1, 10000)/) 
 
  CALL ProcPtr1(U, V)
  IF ( ANY( U%ID .NE. V%ID ) ) STOP 11

  U = (/(DT(20,4)(-I), I=1, 10000)/) 
  U = ProcPtr2(V)
  IF ( ANY( U%ID .NE. V%ID ) ) STOP 12
 
  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  CLASS(DT(*,4)), INTENT(INOUT) :: Arg1(:)
  CLASS(DT(*,4)), INTENT(INOUT) :: Arg2(:)
    SELECT TYPE (Arg1)
    TYPE IS(DT(*,4))
      Arg1 = Arg2
    END SELECT
  END SUBROUTINE

  FUNCTION IntFun(Arg)
  CLASS(DT(:,4)), ALLOCATABLE :: IntFun(:)
  CLASS(DT(*,4))              :: Arg(:)
    ALLOCATE(IntFun(SIZE(Arg)), SOURCE=Arg)
  END FUNCTION 
 
  END

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  CLASS(DT(*,4)) :: Arg1(:)
  CLASS(DT(*,4)) :: Arg2(:)
    SELECT TYPE (Arg1)
    TYPE IS(DT(*,4))
      Arg1 = Arg2
    END SELECT
  END SUBROUTINE

  FUNCTION ExtFun(Arg)
  USE M
  CLASS(DT(:,4)), ALLOCATABLE :: ExtFun(:)
  CLASS(DT(*,4))              :: Arg(:)
    ALLOCATE(ExtFun(SIZE(Arg)), SOURCE=Arg)
  END FUNCTION

