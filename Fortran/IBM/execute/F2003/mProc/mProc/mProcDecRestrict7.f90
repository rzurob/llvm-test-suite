!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcDecRestrict7.f  
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
!*  One has a passed-object dummy arguments and the other does not have a passed obj 
!*  
!* 
!*   
!*  (317253)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT
    CHARACTER(20) :: ID
    CONTAINS
    GENERIC ::  G => ModSub, ExtSub
    PROCEDURE, PASS(ARG2)  :: ModSub
    PROCEDURE, NOPASS      :: ExtSub
  END TYPE


  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT), INTENT(INOUT) :: Arg1
  CLASS(DT), INTENT(IN)    :: Arg2
    Arg1%ID = "ModSub-"//Arg2%ID
  END SUBROUTINE

  SUBROUTINE ExtSub(Arg1, Arg2)
  TYPE(DT), INTENT(INOUT) :: Arg1
  TYPE(DT), INTENT(IN)    :: Arg2
    Arg1%ID = "ExtSub-"//Arg2%ID
  END SUBROUTINE

  END MODULE

  PROGRAM mProcDecRestrict7 
  USE M

  INTERFACE G
    PROCEDURE ModSub
  END INTERFACE


  TYPE(DT) :: T, T1
  
  CALL G(T, DT("0"))
  IF (TRIM(T%ID)    .NE. "ModSub-0"   ) STOP 11

   CALL T%G( T1 )
   IF (TRIM(T1%ID)    .NE. "ModSub-ModSub-0"   ) STOP 12

  CALL T%G(T1, DT("1") )
  IF (TRIM(T1%ID)    .NE. "ExtSub-1"   ) STOP 13


  END



