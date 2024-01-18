! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/kindArg/kindArg/kindArgShape5.f
! opt variations: -qnok -ql

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
!*  Result Value. The value of the result is the shape of SOURCE.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgShape5
  IMPLICIT NONE

  TYPE :: DT(D1)    ! (4)
      INTEGER, KIND :: D1
  END TYPE

  CALL IntSub(RESHAPE((/DT(4)()/),(/1,1,1,1/)))
  CONTAINS

  SUBROUTINE IntSub(K)

  INTEGER :: II(4)=2
  INTEGER :: I0(4)=(/0,2,2,2/)

  CLASS(*) :: K(1:1, 2:2, 4:4, 8:8)
  CLASS(DT(4)), POINTER :: T(:,:,:,:)

  ALLOCATE(T(1:2,2:3, 3:4,4:5))

  IF (ANY( SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=1) )  .NE. II )) ERROR STOP 11
  IF (KIND(SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=1) )) .NE. 1 )   ERROR STOP 12
  IF (ANY( SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=2) )  .NE. II )) ERROR STOP 13
  IF (KIND(SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=2) )) .NE. 2 )   ERROR STOP 14
  IF (ANY( SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=3) )  .NE. II )) ERROR STOP 15
  IF (KIND(SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=3) )) .NE. 4 )   ERROR STOP 16
  IF (ANY( SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=4) )  .NE. II )) ERROR STOP 17
  IF (KIND(SHAPE(SOURCE=T, KIND=UBOUND(K, DIM=4) )) .NE. 8 )   ERROR STOP 18

  IF (ANY( SHAPE(SOURCE=T )                         .NE. II )) ERROR STOP 21
  IF (KIND(SHAPE(SOURCE=T))                         .NE.  4)   ERROR STOP 22


  IF (ANY( SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=1) )  .NE. I0 )) ERROR STOP 31
  IF (KIND(SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=1) )) .NE. 1 )   ERROR STOP 32
  IF (ANY( SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=2) )  .NE. I0 )) ERROR STOP 33
  IF (KIND(SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=2) )) .NE. 2 )   ERROR STOP 34
  IF (ANY( SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=3) )  .NE. I0 )) ERROR STOP 35
  IF (KIND(SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=3) )) .NE. 4 )   ERROR STOP 36
  IF (ANY( SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=4) )  .NE. I0 )) ERROR STOP 37
  IF (KIND(SHAPE(SOURCE=T(1:0,:,:,:), KIND=UBOUND(K, DIM=4) )) .NE. 8 )   ERROR STOP 38

  IF (ANY( SHAPE(SOURCE=T(1:0,:,:,:) )                         .NE. I0 )) ERROR STOP 41
  IF (KIND(SHAPE(SOURCE=T(1:0,:,:,:)))                         .NE.  4)   ERROR STOP 42


  IF (SIZE(SHAPE(SOURCE=T(1,2,4,8), KIND=UBOUND(K, DIM=1) )) .NE. 0 )   ERROR STOP 51
  IF (SIZE(SHAPE(SOURCE=T(1,2,4,8), KIND=UBOUND(K, DIM=2) )) .NE. 0 )   ERROR STOP 52
  IF (SIZE(SHAPE(SOURCE=T(1,2,4,8), KIND=UBOUND(K, DIM=3) )) .NE. 0 )   ERROR STOP 53
  IF (SIZE(SHAPE(SOURCE=T(1,2,4,8), KIND=UBOUND(K, DIM=4) )) .NE. 0 )   ERROR STOP 54

  END SUBROUTINE

  END

