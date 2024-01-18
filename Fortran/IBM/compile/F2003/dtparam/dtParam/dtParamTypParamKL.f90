!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypParamKL   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 27, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type parameters 
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  The type-param-attr-spec explicitly specifies whether a type parameter is 
!*  a kind parameter or a length parameter. 
!*
!*  (ICE-missing err msg)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypParamKL 

  TYPE :: DT0(K, L)
    INTEGER, KIND :: K=0
    INTEGER, LEN  :: L=0
  END TYPE

  TYPE :: DT1(K, L)
    INTEGER, KIND :: K=1
    INTEGER, LEN  :: L=1
    INTEGER(KIND=L)  :: I ! wrong 
    CHARACTER(LEN=K) :: C !ok 
  END TYPE

  TYPE :: DT2(K, L)
    INTEGER, KIND :: K=1
    INTEGER, LEN  :: L=1
    INTEGER(KIND=K)  :: I=L  ! wrong
  END TYPE

  TYPE :: DT3(K, L)
    INTEGER, KIND :: K=1
    INTEGER, LEN  :: L=1
    INTEGER(KIND=K)  :: I=KIND(L) ! ok
  END TYPE

  TYPE (DT0(:, 1))          :: T1  ! wrong
  TYPE (DT0(1, :)), POINTER :: T2  ! ok 

  CONTAINS
  SUBROUTINE S(Arg1, Arg2)
  TYPE(DT0(:, :)), POINTER :: Arg1 ! wrong
  TYPE(DT0(*, :)), POINTER :: Arg2 ! wrong
  END SUBROUTINE
  END


