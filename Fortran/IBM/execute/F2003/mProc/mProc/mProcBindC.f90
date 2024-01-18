!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcBindC.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 03, 2006
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
!*  Bind(C) 
!* 
!*   
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
  USE ISO_C_BINDING

  TYPE, BIND(C) :: DT
    INTEGER(C_INT)    :: ID0
    CHARACTER(C_CHAR) :: ID1
  END TYPE
 
  INTERFACE Fun 
    PROCEDURE ModFun
  END INTERFACE

  PRIVATE ModFun
 
  CONTAINS

  FUNCTION ModFun(Arg)  BIND(C)
  TYPE(DT), INTENT(IN)   :: Arg 
  TYPE(DT)  :: ModFun
    ModFun%ID0 = 0
    ModFun%ID1 = Arg%ID1 
  END FUNCTION 

  FUNCTION ModFun1(Arg, Arg1) BIND(C)
  TYPE(DT), INTENT(IN) :: Arg, Arg1 
  TYPE(DT)  :: ModFun1
    ModFun1%ID0 = 1
    ModFun1%ID1 = Arg%ID1 
  END FUNCTION 

  FUNCTION IntFun(Arg, Arg1, Arg2) BIND(C)
  TYPE(DT), INTENT(IN) :: Arg, Arg1, Arg2 
  TYPE(DT)  :: IntFun
    IntFun = Arg 
  END FUNCTION 

  END MODULE

  FUNCTION ExtFun(Arg, Arg1, Arg2) BIND(C)
  USE M, ONLY : DT
  TYPE(DT), INTENT(IN) :: Arg, Arg1, Arg2 
  TYPE(DT)  :: ExtFun
    ExtFun%ID0 = 2
    ExtFun%ID1 = Arg%ID1 
  END FUNCTION 


  PROGRAM mProcBindC 
  USE M

  PROCEDURE(IntFun)          :: ExtFun 
  PROCEDURE(IntFun), POINTER :: ProcPtr

  INTERFACE Fun
    PROCEDURE ProcPtr 
  END INTERFACE

  PRocPtr => ExtFun

  CALL Check(ModFun1)

  CONTAINS

  SUBROUTINE Check(Proc)
  PROCEDURE(ModFun1) :: Proc

  INTERFACE Fun
    PROCEDURE Proc
  END INTERFACE
 
  TYPE(DT) :: T, T1, T2

  T  = Fun(DT(0, "0"))
  T1 = Fun(DT(1, "1"), DT(1, "1")) 
  T2 = Fun(DT(2, "2"), DT(1, "1"), DT(0, "0")) 
 
  IF ( T%ID0  .NE.   0 )   STOP 11
  IF ( T%ID1  .NE. "0" )   STOP 12

  IF ( T1%ID0  .NE.  1 )   STOP 21
  IF ( T1%ID1  .NE. "1" )  STOP 22

  IF ( T2%ID0  .NE.  2 )   STOP 31
  IF ( T2%ID1  .NE. "2" )  STOP 32

  END SUBROUTINE


  END

