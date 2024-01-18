!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgMminloc2
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 27, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : MINLOC 
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


  PROGRAM kindArgMminloc2

  INTEGER(1),    PARAMETER :: I(3) = (/INTEGER(4)::1,1,1/) 
  CHARACTER,     PARAMETER :: C(3) = (/character::"I","B","M"/) 
  
  PRINT*, MINLOC(C, C .EQ. C, KINDD=I(1))
  PRINT*, MINLOC(ARRAY=C(1), DIM=I(2), KID=kind(1))
  PRINT*, MINLOC(KIND=I(1),ARRAY=C, ARRAY=C, KIND=2)
  PRINT*, MINLOC(ARRAY=C, DIM=2, DIM=1, KIND=1_2)

  PRINT*, MINLOC(C, kInD=I(2)) ! this is fine
  PRINT*, MINLOC(ARRAY=C, MASK=C .NE. " ", dIM=1, kInD=KIND(C)) ! this is fine

  END



