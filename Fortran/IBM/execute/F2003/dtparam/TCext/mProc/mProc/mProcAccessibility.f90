! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/mProc/mProc/mProcAccessibility.f
! opt variations: -qck -qnok

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcAccessibility.f  
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
!*  Accessibility 
!* 
!*   
!*  (317412)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: ID
  END TYPE
 
  PROCEDURE(ModFun1), POINTER  :: ProcPtr

  INTERFACE Fun 
    FUNCTION ExtFun(Arg, Arg1, Arg2)
      IMPORT 
      CLASS(DT(4,*)), INTENT(IN)    :: Arg , Arg1, Arg2
      TYPE(DT(4,20))       :: ExtFun 
    END FUNCTION
    PROCEDURE ExtFun
    PROCEDURE ModFun
    PROCEDURE ProcPtr 
  END INTERFACE
 
   PRIVATE ProcPtr, ModFun, ExtFun

  CONTAINS

  SUBROUTINE ModInit()
    ProcPtr => ModFun1
  END SUBROUTINE

  FUNCTION ModFun(Arg)
  CLASS(DT(4,*)), INTENT(IN)   :: Arg 
  TYPE(DT(4,20))  :: ModFun
    ModFun%ID = "ModFun-" // Arg%ID 
  END FUNCTION 

  FUNCTION ModFun1(Arg, Arg1)
  CLASS(DT(4,*)), INTENT(IN) :: Arg, Arg1 
  TYPE(DT(4,20))  :: ModFun1
    ModFun1%ID = "ModFun1-" // Arg%ID 
  END FUNCTION 

  END MODULE

  FUNCTION ExtFun(Arg, Arg1, Arg2)
  USE M, ONLY : DT
  CLASS(DT(4,*)), INTENT(IN) :: Arg, Arg1, Arg2 
  TYPE(DT(4,20))  :: ExtFun
    ExtFun%ID = "ExtFun-" // Arg%ID 
  END FUNCTION 


  PROGRAM mProcAccessibility 
  USE M

  TYPE(DT(4,20)) :: T, T1, T2

  CALL ModInit()

  T  = Fun(DT(4,20)("0"))
  T1 = Fun(DT(4,20)("00"), DT(4,20)("1")) 
  T2 = Fun(DT(4,20)("000"), DT(4,20)("1"), DT(4,20)("2"))
 
  IF (T%ID     .NE. "ModFun-0" )   STOP 11
  IF (T1%ID    .NE. "ModFun1-00" ) STOP 12
  IF (T2%ID    .NE. "ExtFun-000" ) STOP 13
 
  END

