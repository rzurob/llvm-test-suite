!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefC426
!*
!*  DATE                       : Nov. 29, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Syntax
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  C426 (R431) A parent-type-name shall be the name of a previously defined
!*  extensible type (4.5.6).
!*
!*  (Passing?-313563)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

   TYPE, EXTENDS(DT) :: DT1(K) !?
     INTEGER, KIND :: K
   END TYPE

   TYPE DT
   END TYPE

   TYPE, PRIVATE :: DT3(L)
     INTEGER, LEN :: L
     SEQUENCE
   END TYPE

   TYPE, EXTENDS(DT1) :: DT4(K)
     INTEGER, KIND :: K
   END TYPE

   TYPE, PUBLIC, BIND(C) :: DT5
   END TYPE

   TYPE, EXTENDS(DT5) :: DT6(K)
     INTEGER, KIND :: K
   END TYPE


  END MODULE


  PROGRAM dtParamTypeDefC426

  END

