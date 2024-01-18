!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : 338155 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 19, 2007
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
!*  defect 338155 -- accessible private components in IO from module
!*
!*     
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M 
  TYPE :: DT
    INTEGER, PRIVATE :: I
    INTEGER          :: J
  END TYPE
  END MODULE

  MODULE M1 
  USE M

  TYPE, EXTENDS(DT) :: DT1
  END TYPE

  TYPE(DT1) :: T

  CONTAINS

  SUBROUTINE S()

  print*, T%i 
  ! the component "i" is inaccessible here
  PRINT*, T   
  !? the component "i" is accessible here

  END SUBROUTINE
  END MODULE

  PROGRAM D338155
  END


