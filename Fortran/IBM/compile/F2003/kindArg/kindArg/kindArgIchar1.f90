!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIchar1
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 15, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : ICHAR 
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


  PROGRAM kindArgIchar1

  INTEGER,    PARAMETER :: I(3) = (/-1,0,3/) 
  INTEGER,    PARAMETER :: J(3) = (/1,1,1/) 

  
  PRINT*, ICHAR(C=ACHAR(1_1), KIND=I(1))
  PRINT*, ICHAR(C=ACHAR(1_8), KIND=I(2))
  PRINT*, ICHAR(KIND=I(3), C=ACHAR(10_2))

  PRINT*, ICHAR(IICHAR((/"A","B","C"/)), KIND=0)

  PRINT*, ICHAR(KIND=J(J(J(1))), C=(/"1","2","3"/)) !ok

  END

