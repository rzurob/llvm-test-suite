!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 29, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SHAPE
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGEK(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Result Characteristics. Integer. If KIND is present, the kind type parameter is that specified
!*  by the value of KIND; otherwise the kind type parameter is that of default integer type. The
!*  result is an array of rank one whose size is equal to the rank of SOURCE.
!*
!*  (324742) -modified as 326870
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgShape4
  IMPLICIT NONE

  TYPE :: DT
  END TYPE

  CALL IntSub(RESHAPE((/DT()/),(/1,1,1,1/)))
  CONTAINS

  SUBROUTINE IntSub(K)

  INTEGER :: II(4)=1
  INTEGER :: I0(4)=(/0,1,1,1/)

  !CLASS(DT) :: K(1:, 2:, 4:, 8:)  ! eliminate use of assumed shape array
  CLASS(DT) :: K(1:1, 2:2, 4:4, 8:8)


  IF (ANY( SHAPE(SOURCE=K, KIND=LBOUND(K, DIM=1) )  .NE. II )) STOP 11
  IF (KIND(SHAPE(SOURCE=K, KIND=LBOUND(K, DIM=1) )) .NE. 1 )   STOP 12
  IF (ANY( SHAPE(SOURCE=K, KIND=LBOUND(K, DIM=2) )  .NE. II )) STOP 13
  IF (KIND(SHAPE(SOURCE=K, KIND=LBOUND(K, DIM=2) )) .NE. 2 )   STOP 14
  IF (ANY( SHAPE(SOURCE=K, KIND=LBOUND(K, DIM=3) )  .NE. II )) STOP 15
  IF (KIND(SHAPE(SOURCE=K, KIND=LBOUND(K, DIM=3) )) .NE. 4 )   STOP 16
  IF (ANY( SHAPE(SOURCE=K, KIND=LBOUND(K, DIM=4) )  .NE. II )) STOP 17
  IF (KIND(SHAPE(SOURCE=K, KIND=LBOUND(K, DIM=4) )) .NE. 8 )   STOP 18

  IF (ANY( SHAPE(SOURCE=K )                         .NE. II )) STOP 21
  IF (KIND(SHAPE(SOURCE=K))                         .NE.  4)   STOP 22


  IF (ANY( SHAPE(SOURCE=K(1:0,:,:,:), KIND=LBOUND(K, DIM=1) )  .NE. I0 )) STOP 31
  IF (KIND(SHAPE(SOURCE=K(1:0,:,:,:), KIND=LBOUND(K, DIM=1) )) .NE. 1 )   STOP 32
  IF (ANY( SHAPE(SOURCE=K(1:0,:,:,:), KIND=LBOUND(K, DIM=2) )  .NE. I0 )) STOP 33
  IF (KIND(SHAPE(SOURCE=K(1:0,:,:,:), KIND=LBOUND(K, DIM=2) )) .NE. 2 )   STOP 34
  IF (ANY( SHAPE(SOURCE=K(1:0,:,:,:), KIND=LBOUND(K, DIM=3) )  .NE. I0 )) STOP 35
  IF (KIND(SHAPE(SOURCE=K(1:0,:,:,:), KIND=LBOUND(K, DIM=3) )) .NE. 4 )   STOP 36
  IF (ANY( SHAPE(SOURCE=K(1:0,:,:,:), KIND=LBOUND(K, DIM=4) )  .NE. I0 )) STOP 37
  IF (KIND(SHAPE(SOURCE=K(1:0,:,:,:), KIND=LBOUND(K, DIM=4) )) .NE. 8 )   STOP 38

  IF (ANY( SHAPE(SOURCE=K(1:0,:,:,:) )                         .NE. I0 )) STOP 41
  IF (KIND(SHAPE(SOURCE=K(1:0,:,:,:)))                         .NE.  4)   STOP 42


  IF (SIZE(SHAPE(SOURCE=K(1,2,4,8), KIND=LBOUND(K, DIM=1) )) .NE. 0 )   STOP 51
  IF (SIZE(SHAPE(SOURCE=K(1,2,4,8), KIND=LBOUND(K, DIM=2) )) .NE. 0 )   STOP 52
  IF (SIZE(SHAPE(SOURCE=K(1,2,4,8), KIND=LBOUND(K, DIM=3) )) .NE. 0 )   STOP 53
  IF (SIZE(SHAPE(SOURCE=K(1,2,4,8), KIND=LBOUND(K, DIM=4) )) .NE. 0 )   STOP 54

  END SUBROUTINE

  END

