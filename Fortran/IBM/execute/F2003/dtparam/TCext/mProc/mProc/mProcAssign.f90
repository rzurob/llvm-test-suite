! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/mProc/mProc/mProcAssign.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcAssign.f  
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
!*  A generic interface block specifies a generic interface for each of the
!*  procedures in the interface block. The PROCEDURE statement lists procedure 
!*  pointers, external procedures, du mmy procedures, or module procedures
!*  that have this generic interface. A generic interface is always explicit.
!*  -- Defined assignment 
!*  (316834)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  END TYPE
 
  TYPE, EXTENDS(DT) :: DT1    ! (20,4)
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2    ! (20,4)
  END TYPE

  TYPE, EXTENDS(DT2) :: DT3    ! (20,4)
  END TYPE

  END MODULE

  MODULE M1
  USE M
 
  INTERFACE ASSIGNMENT(=) 
    PROCEDURE ModSub
  END INTERFACE  

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT(*,4)), INTENT(INOUT) :: Arg1 
  TYPE(DT(*,4)), INTENT(IN)    :: Arg2 
    Arg1%ID = Arg2%ID 
  END SUBROUTINE 

  SUBROUTINE ModSub1(Arg1, Arg2)
  TYPE(DT1(*,4)), INTENT(INOUT) :: Arg1 
  TYPE(DT1(*,4)), INTENT(IN)    :: Arg2 
    Arg1 = Arg2 
  END SUBROUTINE 

  SUBROUTINE ModSub2(Arg1, Arg2)
  TYPE(DT2(*,4)), INTENT(INOUT) :: Arg1 
  TYPE(DT2(*,4)), INTENT(IN)    :: Arg2 
    Arg1 = Arg2 
  END SUBROUTINE 

  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(DT3(*,4)), INTENT(INOUT) :: Arg1 
  TYPE(DT3(*,4)), INTENT(IN)    :: Arg2 
    Arg1 = Arg2
  END SUBROUTINE 


  PROGRAM mProcAssign 
  USE M
  USE M1

  INTERFACE ASSIGNMENT(=) 
    SUBROUTINE ExtSub(Arg1, Arg2)
      IMPORT 
      TYPE(DT3(*,4)), INTENT(INOUT) :: Arg1 
      TYPE(DT3(*,4)), INTENT(IN)    :: Arg2 
    END SUBROUTINE 
  END INTERFACE

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ExtSub
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

  TYPE(DT(20,4)) :: T=DT(20,4)(-1)
  TYPE(DT1(20,4)) :: T1=DT1(20,4)(1)
  TYPE(DT2(20,4)) :: T2=DT2(20,4)(2)
  TYPE(DT3(20,4)) :: T3=DT3(20,4)(3)

  ProcPtr => ModSub2


  T  = DT(20,4)(-1)
  T1 = DT1(20,4)(1)
  T2 = DT2(20,4)(2)
  T3 = DT3(20,4)(3)

  IF (T%ID  .NE. -1 ) STOP 11
  IF (T1%ID  .NE. 1 ) STOP 12
  IF (T2%ID  .NE. 2 ) STOP 13
  IF (T3%ID  .NE. 3 ) STOP 14

  END  SUBROUTINE

  END

