! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/mProc/mProc/mProcDecRestrict6.f
! opt variations: -qnock

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcDecRestrict6.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 13, 2006
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
!*  there is a non-passed-object dummy data object in one or the other of them such that
!*  (a) the number of dummy data objects in one that are nonoptional, are not passed-object,
!*     and with which that dummy data object is TKR compatible, possibly including that
!*     dummy data object itself,
!*   exceeds
!*  (b) the number of non-passed-object dummy data objects, both optional and nonoptional,
!*    in the other that are not distinguishable with that dummy data object;
!* 
!*   
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1,N1)    ! (1,20)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: ID
  END TYPE
 
  INTERFACE  GFace 
    PROCEDURE ModFun 
  END INTERFACE

 
  CONTAINS

  FUNCTION ModFun(Arg1, Arg2)
  CLASS(DT(1,*)), INTENT(IN) :: Arg1 
  TYPE(DT(1,*)), INTENT(IN)  :: Arg2 
  TYPE(DT(1,20))              :: ModFun 
    ModFun%ID = "ModFun-"// TRIM(Arg1%ID) // TRIM(Arg2%ID) 
  END FUNCTION 

  FUNCTION ModFun1(Arg)
  TYPE(DT(1,*)), INTENT(IN) :: Arg 
  TYPE(DT(1,20))             :: ModFun1 
    ModFun1%ID = "ModFun1-"//TRIM(Arg%ID)
  END FUNCTION 

  END MODULE


  PROGRAM mProcDecRestrict6 
  USE M

  PROCEDURE(ModFun1), POINTER :: PRocPtr

  INTERFACE  GFace 
    PROCEDURE ProcPtr 
  END INTERFACE


  TYPE(DT(1,20)) :: T
  TYPE(DT(1,20)) :: T1

  ProcPtr => ModFun1


  T  = GFace(DT(1,20)("0"), DT(1,20)("0") )
  T1 = GFace(DT(1,20)("1"))

  IF (TRIM(T%ID)   .NE. "ModFun-00" ) STOP 11
  IF (TRIM(T1%ID)  .NE. "ModFun1-1" ) STOP 12

  
  END

