!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : 340524
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 11, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
!*
!*  REFERENCE                  : Feature Number 289057
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
!*  
!*   340524:
!*   DTP work does cause changes in accessibility.
!*     
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE, PRIVATE :: DT0(K)
    INTEGER, KIND :: K=1
    INTEGER, private :: I=1
  END TYPE

  TYPE, EXTENDS(DT0) :: DT
  END TYPE

  END MODULE

  PROGRAM d340524
  USE M

  TYPE(DT) :: T0

  !PRINT*, T0%DT0%I
  PRINT*, T0%I

  END


