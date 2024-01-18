!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgShape5
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
!*  TARGEK(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!*  Result Value. The value of the result is the shape of SOURCE. 
!*
!*  
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgShape5
  IMPLICIT NONE

  TYPE :: DT
  END TYPE

  CALL IntSub(RESHAPE((/DT()/),(/1,1,1,1/)))
  CONTAINS

  SUBROUTINE IntSub(K)

  INTEGER :: II(4)=2
  INTEGER :: I0(4)=(/0,2,2,2/) 
 
  CLASS(*) :: K(1:1, 2:2, 4:4, 8:8)
  CLASS(DT), POINTER :: T(:,:,:,:)

  ALLOCATE(T(1:2,2:3, 3:4,4:5))

  IF (ANY( SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=1) )  .NE. II )) STOP 11
  IF (KIND(SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=1) )) .NE. 1 )   STOP 12
  IF (ANY( SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=2) )  .NE. II )) STOP 13
  IF (KIND(SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=2) )) .NE. 2 )   STOP 14
  IF (ANY( SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=3) )  .NE. II )) STOP 15
  IF (KIND(SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=3) )) .NE. 4 )   STOP 16
  IF (ANY( SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=4) )  .NE. II )) STOP 17
  IF (KIND(SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=4) )) .NE. 8 )   STOP 18

  IF (ANY( SHAPE(SOURCE=T )                         .NE. II )) STOP 21
  IF (KIND(SHAPE(SOURCE=T))                         .NE.  4)   STOP 22


  IF (ANY( SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=1) )  .NE. I0 )) STOP 31
  IF (KIND(SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=1) )) .NE. 1 )   STOP 32
  IF (ANY( SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=2) )  .NE. I0 )) STOP 33
  IF (KIND(SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=2) )) .NE. 2 )   STOP 34
  IF (ANY( SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=3) )  .NE. I0 )) STOP 35
  IF (KIND(SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=3) )) .NE. 4 )   STOP 36
  IF (ANY( SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=4) )  .NE. I0 )) STOP 37
  IF (KIND(SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=4) )) .NE. 8 )   STOP 38

  IF (ANY( SHAPE(SOURCE=T(1:0,:,:,:) )                         .NE. I0 )) STOP 41
  IF (KIND(SHAPE(SOURCE=T(1:0,:,:,:)))                         .NE.  4)   STOP 42


  IF (SIZE(SHAPE(SOURCE=T(1,2,4,8), KIND=UBOUND(K, DIM=1) )) .NE. 0 )   STOP 51
  IF (SIZE(SHAPE(SOURCE=T(1,2,4,8), KIND=UBOUND(K, DIM=2) )) .NE. 0 )   STOP 52
  IF (SIZE(SHAPE(SOURCE=T(1,2,4,8), KIND=UBOUND(K, DIM=3) )) .NE. 0 )   STOP 53
  IF (SIZE(SHAPE(SOURCE=T(1,2,4,8), KIND=UBOUND(K, DIM=4) )) .NE. 0 )   STOP 54

  END SUBROUTINE

  END

