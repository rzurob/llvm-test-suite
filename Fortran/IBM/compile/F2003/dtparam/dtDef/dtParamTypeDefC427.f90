!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefC427 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 29, 2005
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
!*  C427 (R429) If the type definition contains or inherits (4.5.6.1) a deferred
!*  binding (4.5.4), ABSTRACT shall appear.
!*   
!*
!*  (Complaint on k1 and DSIN? -- 339680) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefC427 
 
   TYPE DT(K0)
     INTEGER, KIND :: K0
   END TYPE

   INTERFACE ABSTRACT
     SUBROUTINE S()
     END SUBROUTINE
   END INTERFACE

   TYPE, EXTENDS(DT) :: DT1(K) 
     INTEGER, KIND :: K
     TYPE(DT(K))   :: Comp 
     CONTAINS
     PROCEDURE(S), DEFERRED, NOPASS :: TBP 
   END TYPE
   
   TYPE, EXTENDS(DT) :: DT2(K) 
     INTEGER, KIND :: K
     CONTAINS
     PROCEDURE(ModSub), NOPASS, DEFERRED :: TBP 
   END TYPE

   PROCEDURE(S) :: Proc

   TYPE, EXTENDS(DT) :: DT3(K) 
     INTEGER, KIND :: K
     CONTAINS
     PROCEDURE(Proc), NOPASS, DEFERRED :: TBP 
   END TYPE

   TYPE :: DT4(K) 
     INTEGER, KIND :: K
     CONTAINS
     PROCEDURE(ABS), NOPASS, DEFERRED :: TBP 
   END TYPE
 
! -------------------------------------------------


   TYPE, ABSTRACT :: DT10(K0)
     INTEGER, KIND :: K0
     CONTAINS
     PROCEDURE(S), DEFERRED, NOPASS :: TBP
   END TYPE

   TYPE, EXTENDS(DT10) :: DT11(K)
     INTEGER, KIND :: K
   END TYPE

   TYPE, ABSTRACT :: DT12(K0)
     INTEGER, KIND :: K0
     CONTAINS
     PROCEDURE(ModSub), NOPASS, DEFERRED :: TBP
   END TYPE

   TYPE, EXTENDS(DT12) :: DT13(K)
     INTEGER, KIND :: K
   END TYPE

   TYPE, EXTENDS(DT), ABSTRACT :: DT14(K1)
     INTEGER, KIND :: K1
     CONTAINS
     PROCEDURE(Proc), NOPASS, DEFERRED :: TBP
   END TYPE

   TYPE, EXTENDS(DT14) :: DT15(K)
     INTEGER, KIND :: K
   END TYPE

   TYPE, ABSTRACT :: DT16(K0)
     INTEGER, KIND :: K0
     CONTAINS
     PROCEDURE(DSIN), NOPASS, DEFERRED :: TBP
   END TYPE
 
   TYPE, EXTENDS(DT16) :: DT17(K)
     INTEGER, KIND :: K
   END TYPE

   CONTAINS

   SUBROUTINE ModSub()
   END SUBROUTINE 
  
  END


