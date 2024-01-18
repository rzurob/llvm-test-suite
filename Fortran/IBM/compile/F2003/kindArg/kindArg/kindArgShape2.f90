!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgShape2
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 29, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : SHAPE 
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
!*  characteristics :: the keyword - KIND 
!*
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgShape2

  INTEGER(1),    PARAMETER :: I(3) = (/1,1,1/) 
  INTEGER(1),    PARAMETER :: Array(3) = (/1,1,1/) 
  INTEGER(1),    PARAMETER :: Source(3)  = (/1,1,2/) 
  
  PRINT*, SHAPE(Array, KINDD=I(1))
  PRINT*, SHAPE(Array, DIM=1, KIND=I(1))
  PRINT*, SHAPE(KIND=Kind(16._16), SOURCE=Source) 
  PRINT*, SHAPE(KIND=KIND(1), SOURCE=Source, KIND=1_2)

  PRINT*, SHAPE(I, kInD=I(2)) ! this is fine
  PRINT*, SHAPE(SOURCE=Source, kInD=KIND(Source)) ! this is fine

  END


