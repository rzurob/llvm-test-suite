! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/mProc/mProc/mProcSourceOfProc.f
! opt variations: -qck -qnok

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcSourceOfProc.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 07, 2006
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
!*  The source of procedures - generic bindings from parent type. 
!*
!*  
!*  (317221)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: ID
  CONTAINS
    GENERIC    :: ASSIGNMENT(=) => ModSub
    PROCEDURE, PASS(Arg2)  :: ModSub
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (4,20)
  END TYPE

  TYPE(DT(4,20)) T

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT(4,*)),          INTENT(OUT)   :: Arg1
  CLASS(DT(4,*)),         INTENT(IN)    :: Arg2
    Arg1%ID = "M-" //Arg2%ID
  END SUBROUTINE

  END MODULE

  MODULE M1
  USE M, ONLY: DT
 
  PROCEDURE(ModSub), POINTER :: ProcPtr

  TYPE, EXTENDS(DT) :: DT1    ! (4,20)
  END TYPE

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT1(4,20)) :: T1=DT1(4,20)("0")
  TYPE(DT1(4,20)) :: T2=DT1(4,20)("2")

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT1(4,*)),          INTENT(OUT)   :: Arg1
  CLASS(DT1(4,*)),         INTENT(IN)    :: Arg2
    Arg1%ID = "M1-" //Arg2%ID
  END SUBROUTINE

  SUBROUTINE IniTProcPtr()
    ProcPtr => ModSub
  END SUBROUTINE

  END MODULE

  PROGRAM  mProcDecRestrict1
  USE M, ONLY: T
  USE M1, ONLY: T1, T2, InitProcPtr, ASSIGNMENT(=)


  CALL InitProcPtr() 

  T  = T1 
  T1 = T2 

  IF (TRIM(T%ID)   .NE. "M-0" )  STOP 11
  IF (TRIM(T1%ID)  .NE. "M1-2" ) STOP 12

  END

