! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mProc/mProc/mProcDecRestrict3.f
! opt variations: -qnok -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcDecRestrict3.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 10, 2006
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
!*  Within a scoping unit, if two procedures have the same generic operator
!*  and the same number of arguments or both define assignment, one shall have
!*  a dummy argument that corresponds by position in the argument list to
!*  to a dummy argument of the other that is distinguishable with it.
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  PROCEDURE(ModSub), POINTER :: ProcPtr

  TYPE :: DT(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  CONTAINS
    GENERIC :: ASSIGNMENT(=) => ModSub
    GENERIC :: OPERATOR( .OK.) => ModFun
    PROCEDURE, PASS(Arg2)  :: ModSub
    PROCEDURE, PASS(Arg2)  :: ModFun
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE (DT(4,*)), INTENT(OUT) :: Arg1
  CLASS(DT(4,*)), INTENT(IN) :: Arg2 
  END SUBROUTINE

  FUNCTION ModFun(Arg1, Arg2)
  TYPE (DT(4,*)), INTENT(IN) :: Arg1
  CLASS(DT(4,*)), INTENT(IN) :: Arg2
  TYPE (DT(4,20))  ModFun
    ModFun = DT(4,20)() 
  END FUNCTION 

  END MODULE
 
  PROGRAM mProcDecRestrict3
  USE M

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ModFun) :: Proc

  INTERFACE ASSIGNMENT(=) 
    PROCEDURE ProcPtr
  END INTERFACE

  INTERFACE OPERATOR( .OK.) 
    PROCEDURE Proc 
  END INTERFACE

  ! the following is ok
  INTERFACE ASSIGNMENT(=) 
    PROCEDURE ModSub
  END INTERFACE

  INTERFACE OPERATOR( .OK.) 
    PROCEDURE ModFun 
  END INTERFACE

  END SUBROUTINE
 
  END


