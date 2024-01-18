! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp /tstdev/F2003/kindArg/kindArg/kindArgShape6.f
! opt variations: -qnok -qnol -qdeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgShape6
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


  PROGRAM kindArgShape6
  IMPLICIT NONE

  TYPE :: DT(D1,N1)    ! (4,20)
      INTEGER, KIND :: D1
      INTEGER, LEN  :: N1
  END TYPE

  CALL IntSub(RESHAPE((/DT(4,20)()/),(/1,1,1,1/)))
  CONTAINS

  SUBROUTINE IntSub(K)

  INTEGER :: II(4)=1
  INTEGER :: I0(4)=(/0,1,1,1/)

  CLASS(*) :: K(1:1, 2:2, 4:4, 8:8)
  CLASS(DT(4,20)), POINTER :: T(:,:,:,:)

  ALLOCATE(T(1:2,2:3, 3:4,4:5))

  IF (ANY( SHAPE(SOURCE=RESHAPE((/K/),(/1,1,1,1/)), KIND=1 )  .NE. II )) STOP 11
  IF (KIND(SHAPE(SOURCE=RESHAPE((/K/),(/1,1,1,1/)), KIND=1 )) .NE. 1 )   STOP 12
  IF (ANY( SHAPE(SOURCE=RESHAPE((/K/),(/1,1,1,1/)), KIND=2 )  .NE. II )) STOP 13
  IF (KIND(SHAPE(SOURCE=RESHAPE((/K/),(/1,1,1,1/)), KIND=2 )) .NE. 2 )   STOP 14
  IF (ANY( SHAPE(SOURCE=RESHAPE((/K/),(/1,1,1,1/)), KIND=4 )  .NE. II )) STOP 15
  IF (KIND(SHAPE(SOURCE=RESHAPE((/K/),(/1,1,1,1/)), KIND=4 )) .NE. 4 )   STOP 16
  IF (ANY( SHAPE(SOURCE=RESHAPE((/K/),(/1,1,1,1/)), KIND=8 )  .NE. II )) STOP 17
  IF (KIND(SHAPE(SOURCE=RESHAPE((/K/),(/1,1,1,1/)), KIND=8 )) .NE. 8 )   STOP 18

  IF (ANY( SHAPE(SOURCE=RESHAPE((/K/),(/1,1,1,1/)) )                         .NE. II )) STOP 21
  IF (KIND(SHAPE(SOURCE=RESHAPE((/K/),(/1,1,1,1/))))                         .NE.  4)   STOP 22


  IF (ANY( SHAPE(SOURCE=RESHAPE((/K/),(/0,1,1,1/)), KIND=1 )  .NE. I0 )) STOP 31
  IF (KIND(SHAPE(SOURCE=RESHAPE((/K/),(/0,1,1,1/)), KIND=1 )) .NE. 1 )   STOP 32
  IF (ANY( SHAPE(SOURCE=RESHAPE((/K/),(/0,1,1,1/)), KIND=2 )  .NE. I0 )) STOP 33
  IF (KIND(SHAPE(SOURCE=RESHAPE((/K/),(/0,1,1,1/)), KIND=2 )) .NE. 2 )   STOP 34
  IF (ANY( SHAPE(SOURCE=RESHAPE((/K/),(/0,1,1,1/)), KIND=4 )  .NE. I0 )) STOP 35
  IF (KIND(SHAPE(SOURCE=RESHAPE((/K/),(/0,1,1,1/)), KIND=4 )) .NE. 4 )   STOP 36
  IF (ANY( SHAPE(SOURCE=RESHAPE((/K/),(/0,1,1,1/)), KIND=8 )  .NE. I0 )) STOP 37
  IF (KIND(SHAPE(SOURCE=RESHAPE((/K/),(/0,1,1,1/)), KIND=8 )) .NE. 8 )   STOP 38

  IF (ANY( SHAPE(SOURCE=RESHAPE((/K/),(/0,1,1,1/)))                          .NE. I0 )) STOP 41
  IF (KIND(SHAPE(SOURCE=RESHAPE((/K/),(/0,1,1,1/))))                         .NE.  4)   STOP 42


  IF (SIZE(SHAPE(SOURCE=1, KIND=1 )) .NE. 0 )   STOP 51
  IF (SIZE(SHAPE(SOURCE=2, KIND=2 )) .NE. 0 )   STOP 52
  IF (SIZE(SHAPE(SOURCE=3, KIND=4 )) .NE. 0 )   STOP 53
  IF (SIZE(SHAPE(SOURCE=4, KIND=8 )) .NE. 0 )   STOP 54

  END SUBROUTINE

  END

