!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcTypBndDefAssgn.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 01, 2006
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
!* Interaction with type bound generic
!* 
!*  -- Defined assignment 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    CHARACTER(20) :: ID
    CONTAINS
    GENERIC    :: ASSIGNMENT(=) => ModSub
    PROCEDURE, PASS(ARG2)  :: ModSub
  END TYPE

  TYPE :: DT1
   CHARACTER(20) :: ID
  END TYPE

  TYPE :: DT2
    CHARACTER(20) :: ID
  END TYPE

  TYPE :: DT3
    CHARACTER(20) :: ID
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT), INTENT(INOUT) :: Arg1 
  CLASS(DT), INTENT(IN)    :: Arg2 
    Arg1%ID = "ModSub-"//Arg2%ID 
  END SUBROUTINE 

  SUBROUTINE ModSub1(Arg1, Arg2)
  TYPE(DT1), INTENT(INOUT) :: Arg1 
  CLASS(DT1), INTENT(IN)    :: Arg2 
    Arg1%ID =  "ModSub1-"//Arg2%ID 
  END SUBROUTINE 

  SUBROUTINE ModSub2(Arg1, Arg2)
  TYPE(DT2), INTENT(INOUT) :: Arg1 
  CLASS(DT2), INTENT(IN)    :: Arg2 
    Arg1%ID = "ModSub2-"//Arg2%ID 
  END SUBROUTINE 

  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(DT3), INTENT(INOUT) :: Arg1 
  TYPE(DT3), INTENT(IN)    :: Arg2 
    Arg1%ID = "ExtSub-"//Arg2%ID
  END SUBROUTINE 


  PROGRAM mProcTypBndDefAssgn 
  USE M

  INTERFACE ASSIGNMENT(=) 
    SUBROUTINE ExtSub(Arg1, Arg2)
      IMPORT 
      TYPE(DT3), INTENT(INOUT) :: Arg1 
      TYPE(DT3), INTENT(IN)    :: Arg2 
    END SUBROUTINE 
  END INTERFACE

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ExtSub
!   PROCEDURE ModSub 
  END INTERFACE

  CALL IntSub(ModSub1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ModSub1)           :: Proc
  PROCEDURE(ModSub2), POINTER  :: ProcPtr

  INTERFACE ASSIGNMENT(=)
    PROCEDURE Proc 
  END INTERFACE

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ProcPtr 
  END INTERFACE

  TYPE(DT)  :: T
  TYPE(DT1) :: T1
  TYPE(DT2) :: T2
  TYPE(DT3) :: T3

  ProcPtr => ModSub2


  T  = DT("0")
  T1 = DT1("1")
  T2 = DT2("2")
  T3 = DT3("3")

  IF (TRIM(T%ID)    .NE. "ModSub-0"   ) STOP 11
  IF (TRIM(T1%ID)   .NE. "ModSub1-1"  ) STOP 12
  IF (TRIM(T2%ID)   .NE. "ModSub2-2"  ) STOP 13
  IF (TRIM(T3%ID)   .NE. "ExtSub-3"   ) STOP 14

  END  SUBROUTINE

  END

