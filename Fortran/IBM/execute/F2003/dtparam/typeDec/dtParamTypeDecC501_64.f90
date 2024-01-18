!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC501_64
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 30, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
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
!*  C501 (R501) In a declaration-type-spec, every type-param-value that is 
!*  not a colon or an asterisk shall be a specification-expr
!*
!*  -- A specification inquiry 
!*  -- IEEE inquiry function(only test 4 of this kind) 
!*    IEEE_SUPPORT_DATATYPE/IEEE_SUPPORT_DENORMAL/IEEE_SUPPORT_DIVIDE/IEEE_SUPPORT_INF 
!* 
!*
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM dtParamTypeDecC501_64
  USE ieee_arithmetic

  TYPE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    INTEGER(K)    :: I(1)=K
    CHARACTER(L)  :: C
  END TYPE

  TYPE(DT0(L=6)) :: T(2:3)

  CALL IntSub()

  CONTAINS

  SUBROUTINE IntSub()

  TYPE, EXTENDS(DT0) :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=K
 
    TYPE(DT0(K=4,       L=SIZE([IEEE_SUPPORT_DATATYPE(1._4)])))     :: T1(1)  
    TYPE(DT0(K=4,       L=SIZE([IEEE_SUPPORT_DENORMAL(-1._8)])))    :: T2(1)  
    TYPE(DT0(K=4,       L=SIZE([IEEE_SUPPORT_DIVIDE(0._16)])))      :: T3(1)  
    TYPE(DT0(K=4,       L=SIZE([IEEE_SUPPORT_INF(0._8)])))          :: T4(1)  
  
  END TYPE

  TYPE (DT(L=8)) :: T1

  IF ( T1%K               .NE.   4          ) STOP 11
  IF ( T1%L               .NE.   8          ) STOP 12
  IF ( T1%KIND            .NE.   4          ) STOP 13  
  IF ( T1%LEN             .NE.   4          ) STOP 14
  IF ( ANY( T1%I          .NE.   4        ) ) STOP 15

  IF ( T1%T1%K            .NE.   4          ) STOP 20
  IF ( T1%T1%L            .NE.   1          ) STOP 21

  IF ( T1%T2%K            .NE.   4          ) STOP 30
  IF ( T1%T2%L            .NE.   1          ) STOP 31

  IF ( T1%T3%K            .NE.   4          ) STOP 40
  IF ( T1%T3%L            .NE.   1          ) STOP 21

  IF ( T1%T4%K            .NE.   4          ) STOP 50
  IF ( T1%T4%L            .NE.   1          ) STOP 51

  END SUBROUTINE

  END

