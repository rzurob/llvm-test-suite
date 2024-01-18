!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrSyntax.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : Syntax 
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
!*  The fisrt test case. 
!*   
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrSyntax
  IMPLICIT NONE

  INTEGER, TARGET  :: I
  INTEGER, TARGET  :: IArr(0:99)=(/(I,I=1,100)/)
  INTEGER, POINTER :: IPtr1(:), IPtr2(:,:)

  IPtr1(1:) => IArr
  IF ( LBOUND(IPtr1, 1) .NE. 1 )   STOP 11 
  IF ( UBOUND(IPtr1, 1) .NE. 100 ) STOP 12 
  IF ( ANY(IPtr1     .NE. (/(I,I=1,100)/)))  STOP 13 

  IPtr2(1:10, 1:10) => IArr
  IF ( ANY(LBOUND(IPtr2) .NE. (/1,1/)) )   STOP 21 
  IF ( ANY(UBOUND(IPtr2) .NE. (/10,10/)))  STOP 22 
  IF ( ANY(IPtr2         .NE. RESHAPE( (/(I,I=1,100)/), (/10,10/))))  STOP 23 

  IPtr2(0:9, 0:9) => IPtr1 
  IF ( ANY(LBOUND(IPtr2) .NE. (/0,0/)) )   STOP 31 
  IF ( ANY(UBOUND(IPtr2) .NE. (/9,9/)))    STOP 32 
  IF ( ANY(IPtr2         .NE. RESHAPE( (/(I,I=1,100)/), (/10,10/))))  STOP 33 

  IPtr2(1:, 0:) => IPtr2 
  IF ( ANY(LBOUND(IPtr2) .NE. (/1,0/)) )    STOP 41 
  IF ( ANY(UBOUND(IPtr2) .NE. (/10,9/)))    STOP 42 
  IF ( ANY(IPtr2         .NE. RESHAPE( (/(I,I=1,100)/), (/10,10/))))  STOP 43 

  END


