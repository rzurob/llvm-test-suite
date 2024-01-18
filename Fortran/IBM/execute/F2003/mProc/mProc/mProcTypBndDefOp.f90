!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcTypBndDefOp.f  
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
!*  interaction with type bound generics
!* 
!*  -- Defined Operator 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    CHARACTER(20) :: ID
  CONTAINS
    GENERIC     :: OPERATOR( + ) => ModFun
    PROCEDURE   :: ModFun
  END TYPE
 
  TYPE, EXTENDS(DT) :: DT1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2
  END TYPE

  TYPE, EXTENDS(DT2) :: DT3
  END TYPE

 
  INTERFACE OPERATOR( + ) 
    PROCEDURE ModFun   ! can be dup
  END INTERFACE  

  CONTAINS

  FUNCTION ModFun(Arg1, Arg2)
  CLASS(DT), INTENT(IN) :: Arg1 
  TYPE(DT), INTENT(IN) :: Arg2 
  TYPE(DT)             :: ModFun 
    ModFun%ID = "ModFun-"// TRIM(Arg1%ID) // TRIM(Arg2%ID) 
  END FUNCTION 

  FUNCTION ModFun1(Arg1, Arg2)
  TYPE(DT1), INTENT(IN) :: Arg1 
  TYPE(DT1), INTENT(IN) :: Arg2 
  TYPE(DT1)             :: ModFun1 
    ModFun1%ID = "ModFun1-"//TRIM(Arg1%ID) // TRIM(Arg2%ID )
  END FUNCTION 

  FUNCTION ModFun2(Arg1, Arg2)
  TYPE(DT2), INTENT(IN) :: Arg1 
  TYPE(DT2), INTENT(IN) :: Arg2 
  TYPE(DT2)             :: ModFun2 
    ModFun2%ID = "ModFun2-"// TRIM(Arg1%ID) // TRIM(Arg2%ID )
  END FUNCTION 

  END MODULE

  FUNCTION ExtFun(Arg1, Arg2)
  USE M
  TYPE(DT3), INTENT(IN) :: Arg1 
  TYPE(DT3), INTENT(IN) :: Arg2 
  TYPE(DT3)             :: ExtFun 
    ExtFun%ID = "ExtFun-"// TRIM(Arg1%ID) // TRIM(Arg2%ID )
  END FUNCTION 


  PROGRAM mProcTypBndDefOp 
  USE M


  INTERFACE  OPERATOR( + ) 
    FUNCTION ExtFun(Arg1, Arg2)
      IMPORT 
      TYPE(DT3), INTENT(IN) :: Arg1 
      TYPE(DT3), INTENT(IN) :: Arg2 
      TYPE(DT3)             :: ExtFun 
    END FUNCTION 
  END INTERFACE

  INTERFACE  OPERATOR( + ) 
    PROCEDURE ExtFun
  END INTERFACE

  CALL IntSub(ModFun1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ModFun1)           :: Proc
  PROCEDURE(ModFun2), POINTER  :: ProcPtr

  INTERFACE  OPERATOR( + ) 
    PROCEDURE Proc 
  END INTERFACE

  INTERFACE  OPERATOR( + ) 
    PROCEDURE ProcPtr 
  END INTERFACE

  TYPE(DT)  :: T
  TYPE(DT1) :: T1
  TYPE(DT2) :: T2
  TYPE(DT3) :: T3

  ProcPtr => ModFun2


  T  = DT("0")  + DT("0") 
  T1 = DT1("1") + DT1("1")
  T2 = DT2("2") + DT2("2")
  T3 = DT3("3") + DT3("3")

  IF (TRIM(T%ID)   .NE. "ModFun-00" )  STOP 11
  IF (TRIM(T1%ID)  .NE. "ModFun1-11" ) STOP 12
  IF (TRIM(T2%ID)  .NE. "ModFun2-22" ) STOP 13
  IF (TRIM(T3%ID)  .NE. "ExtFun-33" )  STOP 14

  END  SUBROUTINE

  END

