!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcSpecificIntF1.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 02, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Generaliztion of PROCEDURE statement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 296676 
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
!*  Any procedure may be referenced via its specific interface if the specific 
!*  interface is accessible. It also may be referenced via a generic interface.
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    INTEGER :: ID
  END TYPE
 
  TYPE, EXTENDS(DT) :: DT1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2
  END TYPE

  TYPE, EXTENDS(DT2) :: DT3
  END TYPE

  END MODULE

  MODULE M1
  USE M

  INTERFACE  OPERATOR( .ADD. ) 
    FUNCTION ExtFun(Arg1, Arg2)
      IMPORT 
      TYPE(DT3), INTENT(IN) :: Arg1 
      TYPE(DT3), INTENT(IN) :: Arg2 
      TYPE(DT3)             :: ExtFun 
    END FUNCTION 
  END INTERFACE
 
  PROCEDURE(ModFun2), POINTER  :: ProcPtr

  INTERFACE OPERATOR( .ADD. ) 
    PROCEDURE ModFun
    MODULE PROCEDURE ModFun1
    PROCEDURE ProcPtr 
  END INTERFACE  

  CONTAINS

  FUNCTION ModFun(Arg1, Arg2)
  TYPE(DT), INTENT(IN) :: Arg1 
  TYPE(DT), INTENT(IN) :: Arg2 
  TYPE(DT)             :: ModFun 
    ModFun%ID = Arg1%ID + Arg2%ID 
  END FUNCTION 

  FUNCTION ModFun1(Arg1, Arg2)
  TYPE(DT1), INTENT(IN) :: Arg1 
  TYPE(DT1), INTENT(IN) :: Arg2 
  TYPE(DT1)             :: ModFun1 
    ModFun1%ID = Arg1%ID + Arg2%ID 
  END FUNCTION 

  FUNCTION ModFun2(Arg1, Arg2)
  TYPE(DT2), INTENT(IN) :: Arg1 
  TYPE(DT2), INTENT(IN) :: Arg2 
  TYPE(DT2)             :: ModFun2 
    ModFun2%ID = Arg1%ID + Arg2%ID 
  END FUNCTION 

  END MODULE

  FUNCTION ExtFun(Arg1, Arg2)
  USE M
  TYPE(DT3), INTENT(IN) :: Arg1 
  TYPE(DT3), INTENT(IN) :: Arg2 
  TYPE(DT3)             :: ExtFun 
    ExtFun%ID = Arg1%ID + Arg2%ID 
  END FUNCTION 


  PROGRAM mProcSpecificIntF1 
  USE M
  USE M1, OPERATOR( .Sub.) => OPERATOR( .ADD. )
  USE M1, Fun => ModFun, Fun1 => ModFun1, Proc => ProcPtr, ExtFun => ExtFun

  TYPE(DT)  :: T=DT(-1)
  TYPE(DT1) :: T1=DT1(1)
  TYPE(DT2) :: T2=DT2(2)
  TYPE(DT3) :: T3=DT3(3)

  Proc => ModFun2

  T  = DT(-1) .SUB. DT(-1) 
  T1 = DT1(1) .SUB. DT1(1)
  T2 = DT2(2) .SUB. DT2(2)
  T3 = DT3(3) .SUB. DT3(3)

  IF (T%ID   .NE. -2 ) STOP 11
  IF (T1%ID  .NE.  2 ) STOP 12
  IF (T2%ID  .NE.  4 ) STOP 13
  IF (T3%ID  .NE.  6 ) STOP 14

  T  = Fun(DT(1), DT(1)) 
  T1 = Fun1(DT1(-1), DT1(-1))
  T2 = Proc(DT2(-2), DT2(-2))
  T3 = ExtFun(DT3(-3), DT3(-3))

  IF (T%ID   .NE.  2 ) STOP 21
  IF (T1%ID  .NE. -2 ) STOP 22
  IF (T2%ID  .NE. -4 ) STOP 23
  IF (T3%ID  .NE. -6 ) STOP 24


  END

