!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgCount1
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 14, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : COUNT 
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


  PROGRAM kindArgCount1

  INTEGER,    PARAMETER :: I(3) = (/-1,0,3/) 
  LOGICAL(8), PARAMETER :: Mask(3) = (/.FALSE., .TRUE., .FALSE./)


  ENUM, BIND(C)
    ENUMERATOR :: ONE=1
  END ENUM
  
  PRINT*, COUNT(MASK=MASK, KIND=I(1))
  PRINT*, COUNT(MASK=MASK, KIND=I(2))
  PRINT*, COUNT(KIND=I(3), MASK=Mask)

  PRINT*, COUNT(MASK=Mask, DIM=1, KIND=7)

  PRINT*, COUNT(KIND=ONE, MASK=Mask) !ok

  END

