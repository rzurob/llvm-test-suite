!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgSize1
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 30, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : SIZE 
!*
!*  REFERENCE                  : Feature Number 289083 
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
!*  characteristics :: value of kind 
!*
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgSize1

  INTEGER,    PARAMETER :: I(3) = (/-1,0,3/) 
  LOGICAL(8), PARAMETER :: Array(3) = (/.FALSE., .TRUE., .FALSE./)

  
  PRINT*, SIZE(ARRAY=ARRAY, KIND=I(1))
  PRINT*, SIZE(ARRAY=ARRAY, KIND=I(2))
  PRINT*, SIZE(KIND=I(3), ARRAY=Array)

  PRINT*, SIZE(ARRAY=Array, DIM=1, KIND=7)

  PRINT*, SIZE(KIND=SIZE(Array)+1, ARRAY=Array) !ok

  END

