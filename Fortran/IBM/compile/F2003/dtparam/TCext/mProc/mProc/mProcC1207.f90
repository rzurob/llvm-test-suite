! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/mProc/mProc/mProcC1207.f
! opt variations: -qnok -ql

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcC1207.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 28, 2006
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
!*  C1207 (R1206) A procedure-name shall have an explicit interface and shall
!*  refer to an accessible procedure pointer, external procedure,
!*  dummy procedure, or module procedure.  
!*
!*  
!*  (316757)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    PROCEDURE(), POINTER, NOPASS :: ProcPtr
  CONTAINS
    PROCEDURE, NOPASS :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub()
    INTERFACE Fun
      PROCEDURE IntSub
    END INTERFACE 
  ENTRY Ent
  CONTAINS
    SUBROUTINE IntSub()
    END SUBROUTINE
  END SUBROUTINE 

  END MODULE

  PROGRAM mProcC1207 
  USE M

  TYPE (DT(4)) :: T

  INTERFACE Fun
    PROCEDURE T%ProcPtr 
    PROCEDURE T%Proc 
  END INTERFACE

  SFun()=0
  INTERFACE Fun
    PROCEDURE SFun 
  END INTERFACE
 
  PROCEDURE(INTEGER) ImpProc
  INTERFACE Fun
    PROCEDURE ImpProc
  END INTERFACE

  INTRINSIC SIN
  INTERFACE Fun
    PROCEDURE SIN 
  END INTERFACE

  END


