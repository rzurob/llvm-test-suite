!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgShape1
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
!*  characteristics :: value of kind 
!*
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgShape1

  END
  
  SUBROUTINE Sub(T)

  INTEGER,    PARAMETER :: I(3) = (/-1,0,11/) 
  INTEGER,    PARAMETER :: J(3) = (/1,1,1/) 

  CLASS(*),   POINTER    :: T(:)
  
  PRINT*, SHAPE(SOURCE=I, KIND=I(1))
  PRINT*, SHAPE(SOURCE=I(:), KIND=I(2))
  PRINT*, SHAPE(KIND=I(3), SOURCE=I)

  PRINT*, SHAPE(SOURCE=I, KIND=SUM(I))

  PRINT*, SHAPE(KIND=KIND(SHAPE(T)), SOURCE=I) !ok

  END SUBROUTINE

