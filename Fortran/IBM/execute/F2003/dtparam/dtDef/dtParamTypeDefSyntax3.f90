!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefSyntax3
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 28, 2005
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Syntax
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
!*  Many type parameters 
!*
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefSyntax3 
  
  TYPE :: DT1(   &
    & K1, &
    & K2, &
    & K3, &
    & K4, &
    & K5, &
    & K6, &
    & K7, &
    & K8, &
    & K9, &
    & K10, &
    & K11, &
    & K12, &
    & K13, &
    & K14, &
    & K15, &
    & K16, &
    & K17, &
    & K18, &
    & K19, &
    & K20, &
    & K21, &
    & K22, &
    & K23, &
    & K24, &
    & K25, &
    & K26, &
    & K27, &
    & K28, &
    & K29, &
    & K30, &
    & K31, &
    & K32  )  

    INTEGER, KIND :: K1
    INTEGER, KIND :: K2
    INTEGER, KIND :: K3
    INTEGER, KIND :: K4
    INTEGER, KIND :: K5
    INTEGER, KIND :: K6
    INTEGER, KIND :: K7
    INTEGER, KIND :: K8
    INTEGER, KIND :: K9
    INTEGER, KIND :: K10
    INTEGER, KIND :: K11
    INTEGER, KIND :: K12
    INTEGER, KIND :: K13
    INTEGER, KIND :: K14
    INTEGER, KIND :: K15
    INTEGER, KIND :: K16
    INTEGER, KIND :: K17
    INTEGER, KIND :: K18
    INTEGER, KIND :: K19
    INTEGER, KIND :: K20
    INTEGER, KIND :: K21
    INTEGER, KIND :: K22
    INTEGER, KIND :: K23
    INTEGER, KIND :: K24
    INTEGER, KIND :: K25
    INTEGER, KIND :: K26
    INTEGER, KIND :: K27
    INTEGER, KIND :: K28
    INTEGER, KIND :: K29
    INTEGER, KIND :: K30
    INTEGER, KIND :: K31
    INTEGER, KIND :: K32
    
  END TYPE

  
  TYPE :: DT2(   &
    & L1, &
    & L2, &
    & L3, &
    & L4, &
    & L5, &
    & L6, &
    & L7, &
    & L8, &
    & L9, &
    & L10, &
    & L11, &
    & L12, &
    & L13, &
    & L14, &
    & L15, &
    & L16, &
    & L17, &
    & L18, &
    & L19, &
    & L20, &
    & L21, &
    & L22, &
    & L23, &
    & L24, &
    & L25, &
    & L26, &
    & L27, &
    & L28, &
    & L29, &
    & L30, &
    & L31, &
    & L32  )

    INTEGER, LEN  :: L1
    INTEGER, LEN :: L2
    INTEGER, LEN :: L3
    INTEGER, LEN :: L4
    INTEGER, LEN :: L5
    INTEGER, LEN :: L6
    INTEGER, LEN :: L7
    INTEGER, LEN :: L8
    INTEGER, LEN :: L9
    INTEGER, LEN :: L10
    INTEGER, LEN :: L11
    INTEGER, LEN :: L12
    INTEGER, LEN :: L13
    INTEGER, LEN :: L14
    INTEGER, LEN :: L15
    INTEGER, LEN :: L16
    INTEGER, LEN :: L17
    INTEGER, LEN :: L18
    INTEGER, LEN :: L19
    INTEGER, LEN :: L20
    INTEGER, LEN :: L21
    INTEGER, LEN :: L22
    INTEGER, LEN :: L23
    INTEGER, LEN :: L24
    INTEGER, LEN :: L25
    INTEGER, LEN :: L26
    INTEGER, LEN :: L27
    INTEGER, LEN :: L28
    INTEGER, LEN :: L29
    INTEGER, LEN :: L30
    INTEGER, LEN :: L31
    INTEGER, LEN :: L32
    
  END TYPE

  TYPE (DT1 (          &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8))  T1

  TYPE (DT2(           &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8, &
         & 1, 2, 4, 8))  T2

  IF ( T1%K1 .NE. 1 .OR. T2%L1 .NE. 1 ) STOP 11
  IF ( T1%K2 .NE. 2 .OR. T2%L2 .NE. 2 ) STOP 12
  IF ( T1%K3 .NE. 4 .OR. T2%L3 .NE. 4 ) STOP 13
  IF ( T1%K4 .NE. 8 .OR. T2%L4 .NE. 8 ) STOP 14
 
  IF ( T1%K5 .NE. 1 .OR. T2%L5 .NE. 1 ) STOP 21
  IF ( T1%K6 .NE. 2 .OR. T2%L6 .NE. 2 ) STOP 22
  IF ( T1%K7 .NE. 4 .OR. T2%L7 .NE. 4 ) STOP 23
  IF ( T1%K8 .NE. 8 .OR. T2%L8 .NE. 8 ) STOP 24
 
  IF ( T1%K9  .NE. 1 .OR. T2%L9  .NE. 1 ) STOP 31
  IF ( T1%K10 .NE. 2 .OR. T2%L10 .NE. 2 ) STOP 32
  IF ( T1%K11 .NE. 4 .OR. T2%L11 .NE. 4 ) STOP 33
  IF ( T1%K12 .NE. 8 .OR. T2%L12 .NE. 8 ) STOP 34
 
  IF ( T1%K13 .NE. 1 .OR. T2%L13 .NE. 1 ) STOP 41
  IF ( T1%K14 .NE. 2 .OR. T2%L14 .NE. 2 ) STOP 42
  IF ( T1%K15 .NE. 4 .OR. T2%L15 .NE. 4 ) STOP 43
  IF ( T1%K16 .NE. 8 .OR. T2%L16 .NE. 8 ) STOP 44
 
  IF ( T1%K17 .NE. 1 .OR. T2%L17 .NE. 1 ) STOP 51
  IF ( T1%K18 .NE. 2 .OR. T2%L18 .NE. 2 ) STOP 52
  IF ( T1%K19 .NE. 4 .OR. T2%L19 .NE. 4 ) STOP 53
  IF ( T1%K20 .NE. 8 .OR. T2%L20 .NE. 8 ) STOP 54
 
  IF ( T1%K21 .NE. 1 .OR. T2%L21 .NE. 1 ) STOP 61
  IF ( T1%K22 .NE. 2 .OR. T2%L22 .NE. 2 ) STOP 62
  IF ( T1%K23 .NE. 4 .OR. T2%L23 .NE. 4 ) STOP 63
  IF ( T1%K24 .NE. 8 .OR. T2%L24 .NE. 8 ) STOP 64
 
  IF ( T1%K25 .NE. 1 .OR. T2%L25 .NE. 1 ) STOP 71
  IF ( T1%K26 .NE. 2 .OR. T2%L26 .NE. 2 ) STOP 72
  IF ( T1%K27 .NE. 4 .OR. T2%L27 .NE. 4 ) STOP 73
  IF ( T1%K28 .NE. 8 .OR. T2%L28 .NE. 8 ) STOP 74
 
  IF ( T1%K29 .NE. 1 .OR. T2%L29 .NE. 1 ) STOP 81
  IF ( T1%K30 .NE. 2 .OR. T2%L30 .NE. 2 ) STOP 82
  IF ( T1%K31 .NE. 4 .OR. T2%L31 .NE. 4 ) STOP 83
  IF ( T1%K32 .NE. 8 .OR. T2%L32 .NE. 8 ) STOP 84
 
  END

