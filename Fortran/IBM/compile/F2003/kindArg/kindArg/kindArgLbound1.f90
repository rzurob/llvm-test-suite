!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLbound1
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 21, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : LBOUND 
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
!*  (324719) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLbound1

  INTEGER,    PARAMETER :: I(3) = (/-1,0,3/) 
  INTEGER,    PARAMETER :: J(3) = (/1,1,1/) 

  
  PRINT*, LBOUND(ARRAY=I, KIND=I(1))
  PRINT*, LBOUND(ARRAY=I(:), KIND=I(2))
  PRINT*, LBOUND(KIND=I(3), DIM=I(1), ARRAY=I)

  PRINT*, LBOUND(ARRAY=I, KIND=SUM(I)) !ok

  PRINT*, LBOUND(KIND=SUM(I), ARRAY=I) !ok

  END

