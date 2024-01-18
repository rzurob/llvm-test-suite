!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a reference to an elemental intrinsic
!*
!*  -  NEAREST
!*  (319219)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemNEAREST
  IMPLICIT NONE
  INTEGER :: I, J



  REAL(KIND( NEAREST(X=(/(REAL(I, KIND=4), I=1,128)/), S=-1._8)))  ::    &
  TR4(128) = NEAREST(X=(/(REAL(I, KIND=4), I=1,128)/), S=-1._8)

  REAL(KIND( NEAREST(X=(/(REAL(I, KIND=8), I=1,128)/), S=-1._4)))  ::    &
  TR8(128) = NEAREST(X=(/(REAL(I, KIND=8), I=1,128)/), S=-1._4)

  REAL(KIND( NEAREST(X=(/(REAL(I, KIND=16), I=1,128)/), S=-1._8))) ::    &
  TR6(128) = NEAREST(X=(/(REAL(I, KIND=16), I=1,128)/), S=-1._8)

  REAL(4) :: R4(128)
  REAL(8) :: R8(128)
  REAL(16) :: R6(128)

  REAL(4) :: X4(128) = EOSHIFT( NEAREST(X=(/(REAL(I, KIND=4), I=1,128)/), S=-1._4), SHIFT=-1, BOUNDARY=NEAREST(0.0_4, -1.0))
  REAL(8) :: X8(128) = EOSHIFT( NEAREST(X=(/(REAL(I, KIND=8), I=1,128)/), S=-1._8), SHIFT=-1, BOUNDARY=NEAREST(0.0_8, -1.0))
  REAL(16):: X6(128) = EOSHIFT( NEAREST(X=(/(REAL(I, KIND=16),I=1,128)/), S=-1._16), SHIFT=-1, BOUNDARY=NEAREST(0.0_16, -1.0))



  R4 = NEAREST(X=(/(REAL(I, KIND=4), I=1,128)/), S=-1._8)
  R8 = NEAREST(X=(/(REAL(I, KIND=8), I=1,128)/), S=-1._4)
  R6 = NEAREST(X=(/(REAL(I, KIND=16),I=1,128)/), S=-1._8)



  IF ( KIND(TR4)  .NE.    4 )  STOP 11
  IF ( KIND(TR8)  .NE.    8 )  STOP 12
  IF ( KIND(TR6)  .NE.    16)  STOP 13

  IF ( ANY (TR4   .NE.   R4 ))  STOP 21
  IF ( ANY (TR8   .NE.   R8 ))  STOP 22
  IF ( ANY (TR6   .NE.   R6 ))  STOP 23

  IF ( ANY (X4   .NE.   NEAREST(X=(/(REAL(I, KIND=4), I=0,127)/), S=-1._4)))   STOP 31
  IF ( ANY (X8   .NE.   NEAREST(X=(/(REAL(I, KIND=8), I=0,127)/), S=-1._8)))   STOP 32
  IF ( ANY (X6   .NE.   NEAREST(X=(/(REAL(I, KIND=16),I=0,127)/), S=-1._16)))  STOP 33


  END


