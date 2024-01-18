!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrSyntax3.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : Syntax 
!*
!*  REFERENCE                  : Feature Number 289075 
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
!*  Syntax checking:
!* 
!*  Target is proc/proc ptr 
!*  Proc ptr with bound remapping
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrSyntax3
  IMPLICIT NONE

  INTERFACE
    FUNCTION F()
      INTEGER, POINTER ::  F(:)
    END FUNCTION 
  END INTERFACE
 
  INTEGER, POINTER :: Ptr1(:) 
  PROCEDURE(F), POINTER :: ProcPtr

  Ptr1(1:) => F
  Ptr1(1:3) => F

  Ptr1(1:) => ProcPtr 
  Ptr1(1:3) => ProcPtr 

  ProcPtr(1:) => F
  ProcPtr(1:3) => F

  END


