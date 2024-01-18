!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgCount4
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 15, 2006
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
!*  Count the number of true elements of MASK along dimension DIM 
!*    
!*  () 
!*
!234567890153456789015345678901534567890153456789015345678901534567890


  PROGRAM kindArgCount4
  IMPLICIT NONE
  
  INTEGER(1) :: I1
  LOGICAL(1) :: L1(4,4)
  INTEGER(2) :: I2
  LOGICAL(2) :: L2(4,4)
  INTEGER(4) :: I4
  LOGICAL(4) :: L4(4,4)
  INTEGER(8) :: I8
  LOGICAL(8) :: L8(4,4)
    
  INTEGER    :: Cnt1(4)=(/3,4,4,4/)
  INTEGER    :: Cnt2(4)=(/4,3,4,4/)
  INTEGER    :: Cnt3(4)=(/4,4,3,4/)
  INTEGER    :: Cnt4(4)=(/4,4,4,3/)
 
  L1= .TRUE.;L1(1,1)=.FALSE.
  L2= .TRUE.;L2(2,2)=.FALSE.
  L4= .TRUE.;L4(3,3)=.FALSE.
  L8= .TRUE.;L8(4,4)=.FALSE.

  IF (ANY(COUNT(RESHAPE((/L1/), (/4,4/)), KIND=KIND(L1), DIM=1_1 ).NE. Cnt1 ))  STOP 11
  IF (ANY(COUNT(RESHAPE((/L1/), (/4,4/)), DIM=1_1)                .NE. Cnt1 )) STOP 15
  IF (COUNT(RESHAPE((/L1/), (/4,4/)), KIND=KIND(L1))          .NE. 15 ) STOP 13
  IF (COUNT(RESHAPE((/L1/), (/4,4/))  )                       .NE. 15 ) STOP 14

  IF (ANY(COUNT(RESHAPE((/L2/), (/4,4/)), KIND=KIND(L2), DIM=2_2 ).NE. Cnt2 ))  STOP 21
  IF (ANY(COUNT(RESHAPE((/L2/), (/4,4/)), DIM=2_2)                .NE. Cnt2 )) STOP 22
  IF (COUNT(RESHAPE((/L2/), (/4,4/)), KIND=KIND(L2))          .NE. 15 ) STOP 23
  IF (COUNT(RESHAPE((/L2/), (/4,4/))  )                       .NE. 15 ) STOP 24

  IF (ANY(COUNT(RESHAPE((/L4/), (/4,4/)), KIND=KIND(L1), DIM=1_4 ).NE. Cnt3 ))  STOP 41
  IF (ANY(COUNT(RESHAPE((/L4/), (/4,4/)), DIM=2_4)                .NE. Cnt3 )) STOP 42
  IF (COUNT(RESHAPE((/L4/), (/4,4/)), KIND=KIND(L4))          .NE. 15 ) STOP 43
  IF (COUNT(RESHAPE((/L4/), (/4,4/))  )                       .NE. 15 ) STOP 44

  IF (ANY(COUNT(RESHAPE((/L8/), (/4,4/)), KIND=KIND(L8), DIM=1_8 ).NE. Cnt4 ))  STOP 81
  IF (ANY(COUNT(RESHAPE((/L8/), (/4,4/)), DIM=1_8)                .NE. Cnt4 )) STOP 82
  IF (COUNT(RESHAPE((/L8/), (/4,4/)), KIND=KIND(L8))          .NE. 15 ) STOP 83
  IF (COUNT(RESHAPE((/L8/), (/4,4/))  )                       .NE. 15 ) STOP 84



  END

