!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrBoundSpec.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289075 
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
!*  R737 bounds-spec is lower-bound-expr :
!*   
!*
!*  (322952)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrBoundSpec 
  IMPLICIT NONE

  INTEGER(1), PARAMETER :: I1Max=127
  INTEGER(1), PARAMETER :: I1Min=-128

  INTEGER(2), PARAMETER :: I2Max=32767
  INTEGER(2), PARAMETER :: I2Min=-32768

  INTEGER(4), PARAMETER :: I4Max=2147483647
  INTEGER(4), PARAMETER :: I4Min=-2147483648

  INTEGER(8), PARAMETER :: I8Max=9223372036854775807_8
  INTEGER(8), PARAMETER :: I8Min=-9223372036854775808_8

  TYPE :: DT0
  END TYPE

  TYPE :: DT
    TYPE(DT0), POINTER :: IPtr1(:), IPtr2(:,:)
  END TYPE

  INTEGER, TARGET  :: I

  TYPE(DT0), TARGET  :: I1Arr(I2Min:I2Max)=(/(DT0(),I=I2Min, I2Max)/)
  TYPE(DT0), TARGET  :: I2Arr(I1Min:-1, I1Max) =  &
                      &  RESHAPE((/(DT0(),I=1, I1Max*(I1Max+1))/), (/I1Max+1, I1Max+0/))

  TYPE(DT) :: T

  T%IPtr1(I1Min:) => I1Arr
  IF ( ANY(LBOUND(T%IPtr1) .NE. (/I1Min/)) )   STOP 9 
  IF ( ANY(UBOUND(T%IPtr1) .NE. (/(I2Max+1)*2+i1Min-1/)))    STOP 10 

  T%IPtr1(I2Max:) => I1Arr
  IF ( ANY(LBOUND(T%IPtr1) .NE. (/I2Max/)) )                 STOP 11 
  IF ( ANY(UBOUND(T%IPtr1) .NE. (/(I2Max+1)*2+I2Max-1/)))    STOP 12 

  T%IPtr1(I4Max:) => I1Arr
  IF ( ANY(LBOUND(T%IPtr1) .NE. (/I4Max/)) )                 STOP 21 
  IF ( ANY(UBOUND(T%IPtr1) .NE. (/(I2Max+1)*2+I4Max-1/)))    STOP 22 

  T%IPtr2(1_8:, 0_8:) => I2Arr
  IF ( ANY(LBOUND(T%IPtr2) .NE. (/1_8, 0_8/)) )              STOP 31 
  IF ( ANY(UBOUND(T%IPtr2) .NE. (/I1Max+1, I1Max-1/)))       STOP 32 

  

  T%IPtr2(I1Min:, I1Min:) => I2Arr
  IF ( ANY(LBOUND(T%IPtr2) .NE. (/I1Min, I1Min/)) )   STOP 41 
  IF ( ANY(UBOUND(T%IPtr2) .NE. (/-1, -2/)))          STOP 42 

  T%IPtr2(I2Max:, I2Max:) => I2Arr
  IF ( ANY(LBOUND(T%IPtr2) .NE. (/I2Max, I2Max/)) )               STOP 51
  IF ( ANY(UBOUND(T%IPtr2) .NE. (/I2Max+I1Max+0, I2Max+I1Max-1/)))STOP 52 

  END


