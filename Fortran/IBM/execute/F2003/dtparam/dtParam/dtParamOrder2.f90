!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamOrder2   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 22, 2006
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
!*  he type parameter order of an extended type consists of the type parameter order of
!*  its parent type followed by any additional type parameters in the order of the type
!*  parameter list in the derived-type definition. 
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamOrder2 
  IMPLICIT NONE

  TYPE DT1(K1, K2)
    INTEGER,          KIND :: K1
    INTEGER(KIND=K1), KIND :: K2=K1
  END TYPE


  TYPE, EXTENDS(DT1) ::  DT2(L1, L2)
    INTEGER(KIND=K2), LEN :: L2=K2
    INTEGER(KIND=K1), LEN :: L1=K1
    INTEGER(KIND=1)      :: Arr(L1:L2, L1:L2)
  END TYPE

  TYPE(DT2(2,4)) :: T1
  TYPE(DT2(2,4,-1)) :: T2
  TYPE(DT2(2,4,0,0)) :: T3
  TYPE(DT2(2,L2=2)) :: T4

  IF (KIND(T1%K1) .NE. 4 )                 STOP 10
  IF (     T1%K1  .NE. 2 )                 STOP 11
  IF (KIND(T1%K2) .NE. 2 )                 STOP 12
  IF (     T1%K2  .NE. 4 )                 STOP 13
  IF (KIND(T1%L1) .NE. 2 )                 STOP 14
  IF (     T1%L1  .NE. 2 )                 STOP 15
  IF (KIND(T1%L2) .NE. 4 )                 STOP 16
  IF (     T1%L2  .NE. 4 )                 STOP 17
  IF (ANY(LBOUND(T1%Arr) .NE. (/2,2/) ))   STOP 18
  IF (ANY(UBOUND(T1%Arr) .NE. (/4,4/) ))   STOP 19

  IF (KIND(T2%K1) .NE. 4 )                 STOP 20
  IF (     T2%K1  .NE. 2 )                 STOP 21
  IF (KIND(T2%K2) .NE. 2 )                 STOP 22
  IF (     T2%K2  .NE. 4 )                 STOP 23
  IF (KIND(T2%L1) .NE. 2 )                 STOP 24
  IF (     T2%L1  .NE.-1 )                 STOP 25
  IF (KIND(T2%L2) .NE. 4 )                 STOP 26
  IF (     T2%L2  .NE. 4 )                 STOP 27
  IF (ANY(LBOUND(T2%Arr) .NE. (/-1,-1/) )) STOP 28
  IF (ANY(UBOUND(T2%Arr) .NE. (/4,  4/) )) STOP 29

  IF (KIND(T3%K1) .NE. 4 )                 STOP 30
  IF (     T3%K1  .NE. 2 )                 STOP 31
  IF (KIND(T3%K2) .NE. 2 )                 STOP 32
  IF (     T3%K2  .NE. 4 )                 STOP 33
  IF (KIND(T3%L1) .NE. 2 )                 STOP 34
  IF (     T3%L1  .NE. 0 )                 STOP 35
  IF (KIND(T3%L2) .NE. 4 )                 STOP 36
  IF (     T3%L2  .NE. 0 )                 STOP 37
  IF (ANY(LBOUND(T3%Arr) .NE. (/ 0, 0/) )) STOP 38
  IF (ANY(UBOUND(T3%Arr) .NE. (/ 0, 0/) )) STOP 39

  IF (KIND(T4%K1) .NE. 4 )                 STOP 40
  IF (     T4%K1  .NE. 2 )                 STOP 41
  IF (KIND(T4%K2) .NE. 2 )                 STOP 42
  IF (     T4%K2  .NE. 2 )                 STOP 43
  IF (KIND(T4%L1) .NE. 2 )                 STOP 44
  IF (     T4%L1  .NE. 2 )                 STOP 45
  IF (KIND(T4%L2) .NE. 2 )                 STOP 46
  IF (     T4%L2  .NE. 2 )                 STOP 47
  IF (ANY(LBOUND(T4%Arr) .NE. (/ 2, 2/) )) STOP 48
  IF (ANY(UBOUND(T4%Arr) .NE. (/ 2, 2/) )) STOP 49



  END


