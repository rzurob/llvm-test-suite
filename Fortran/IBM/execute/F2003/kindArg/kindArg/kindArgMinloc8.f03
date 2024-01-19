!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 27, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : MINLOC
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Result Value.
!*  Case (iii): If ARRAY has rank one, MINLOC (ARRAY, DIM = DIM [, MASK = MASK]) is
!*  a scalar whose value is equal to that of the first element of MINLOC (ARRAY [,
!*  MASK = MASK]). Otherwise, the value of element (s1, s2, ..., sDIM-1, sDIM+1,
!*  ..., sn) of the result is equal to
!*  MINLOC (ARRAY (s1, s2, ..., sDIM-1, :, sDIM+1, ..., sn), DIM=1
!*  [, MASK = MASK (s1, s2, ..., sDIM-1, :, sDIM+1, ..., sn) ] ).
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMinloc8
  IMPLICIT NONE

  INTEGER     :: I
  INTEGER(1)  :: II1(-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1)  = 0
  INTEGER(2)  :: II2(-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1)  = 0
  INTEGER(4)  :: II4(-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1)  = 0
  INTEGER(8)  :: II8(-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1)  = 0

  REAL(4)     :: RR4(-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1)  = 0
  REAL(8)     :: RR8(-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1)  = 0
  REAL(16)    :: RR6(-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1)  = 0

  CHARACTER   :: CC(-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1)   = ACHAR(0)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  LOGICAL      :: MM (-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1)=.TRUE.
  LOGICAL      :: MM1(-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1,-2:-1)=.FALSE.


  II1 = 1
  II1(-1,:,:,:,:,:,:,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=II1, MASK=MM, DIM=1, KIND=II1%KIND )  .NE. 2)) ERROR STOP 10
  IF (KIND(MINLOC(ARRAY=II1, MASK=MM, DIM=1, KIND=II1%KIND )) .NE. 1)  ERROR STOP 11

  II2 = 1
  II2(:,-1,:,:,:,:,:,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=II2, MASK=MM, DIM=2, KIND=II2%KIND )  .NE. 2)) ERROR STOP 12
  IF (KIND(MINLOC(ARRAY=II2, MASK=MM, DIM=2, KIND=II2%KIND )) .NE. 2)  ERROR STOP 13

  II4 = 1
  II4(:,:,-1,:,:,:,:,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=II4, MASK=MM, DIM=3, KIND=II4%KIND )  .NE. 2)) ERROR STOP 14
  IF (KIND(MINLOC(ARRAY=II4, MASK=MM, DIM=3, KIND=II4%KIND )) .NE. 4)  ERROR STOP 15

  II8 = 1
  II8(:,:,:,-1,:,:,:,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=II8, MASK=MM, DIM=4, KIND=II1%KIND )  .NE. 2)) ERROR STOP 16
  IF (KIND(MINLOC(ARRAY=II8, MASK=MM, DIM=4, KIND=II1%KIND )) .NE. 1)  ERROR STOP 17


  RR4 = 1
  RR4(:,:,:,:,-1,:,:,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=RR4(:,:,:,:,:,:,:,:,:,:), MASK=MM, DIM=5, KIND=RR4(:,:,:,:,-2,:,:,:,:,:)%KIND ) .NE. 2)) ERROR STOP 21
  IF (KIND(MINLOC(ARRAY=RR4(:,:,:,:,:,:,:,:,:,:), MASK=MM, DIM=5, KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 4)  ERROR STOP 22

  RR8 = 1
  RR8(:,:,:,:,:,-1,:,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=RR8(:,:,:,:,:,:,:,:,:,:), MASK=MM, DIM=6, KIND=RR8(:,:,:,:,:,-2,:,:,:,:)%KIND ) .NE. 2)) ERROR STOP 23
  IF (KIND(MINLOC(ARRAY=RR8(:,:,:,:,:,:,:,:,:,:), MASK=MM, DIM=6, KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 8)  ERROR STOP 24

  RR6 = 1
  RR6(:,:,:,:,:,:,-1,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:), MASK=MM, DIM=7, KIND=RR6(:,:,:,:,:,:,-2,:,:,:)%KIND-15 ) .NE. 2)) ERROR STOP 25
  IF (KIND(MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:), MASK=MM, DIM=7, KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-15 )) .NE.  1) ERROR STOP 26


  CC = ACHAR(1)
  CC(:,:,:,:,:,:,:,-1,:,:) = ACHAR(0)
  IF (ANY( MINLOC(ARRAY=CC, MASK=MM1, DIM=8, KIND=CC%KIND )  .NE. 0))   ERROR STOP 30
  IF (KIND(MINLOC(ARRAY=CC, MASK=MM1, DIM=8, KIND=CC%KIND )) .NE. 1)    ERROR STOP 31


  II1 = 1
  II1(:,:,:,:,:,:,:,:,-1,:) = 0
  IF (ANY( MINLOC(ARRAY=II1, MASK=MM1, DIM=9, KIND=II1%KIND )  .NE. 0)) ERROR STOP 40
  IF (KIND(MINLOC(ARRAY=II1, MASK=MM1, DIM=9, KIND=II1%KIND )) .NE. 1)  ERROR STOP 41

  II2 = 1
  II2(:,:,:,:,:,:,:,:,:,-1) = 0
  IF (ANY( MINLOC(ARRAY=II2, MASK=MM1, DIM=10, KIND=II2%KIND )  .NE. 0)) ERROR STOP 42
  IF (KIND(MINLOC(ARRAY=II2, MASK=MM1, DIM=10, KIND=II2%KIND )) .NE. 2)  ERROR STOP 43

  II4 = 1
  II4(:,:,-1,:,:,:,:,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=II4, MASK=MM1, DIM=3, KIND=II4%KIND )  .NE. 0)) ERROR STOP 44
  IF (KIND(MINLOC(ARRAY=II4, MASK=MM1, DIM=3, KIND=II4%KIND )) .NE. 4)  ERROR STOP 45

  II8 = 1
  II8(:,:,:,-1,:,:,:,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=II8, MASK=MM1, DIM=4, KIND=II8%KIND )  .NE. 0)) ERROR STOP 46
  IF (KIND(MINLOC(ARRAY=II8, MASK=MM1, DIM=4, KIND=II8%KIND )) .NE. 8)  ERROR STOP 47


  RR4 = 1
  RR4(:,:,:,:,-1,:,:,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=RR4(:,:,:,:,:,:,:,:,:,:), MASK=MM1, DIM=5, KIND=RR4(:,:,:,:,-1,:,:,:,:,:)%KIND ) .NE. 0)) ERROR STOP 61
  IF (KIND(MINLOC(ARRAY=RR4(:,:,:,:,:,:,:,:,:,:), MASK=MM1, DIM=5, KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 4)  ERROR STOP 62

  RR8 = 1
  RR8(:,:,:,:,:,-1,:,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=RR8(:,:,:,:,:,:,:,:,:,:), MASK=MM1, DIM=6, KIND=RR8(:,:,:,:,:,-1,:,:,:,:)%KIND ) .NE. 0)) ERROR STOP 63
  IF (KIND(MINLOC(ARRAY=RR8(:,:,:,:,:,:,:,:,:,:), MASK=MM1, DIM=6, KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 8)  ERROR STOP 64

  RR6 = 1
  RR6(:,:,:,:,:,:,-1,:,:,:) = 0
  IF (ANY( MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:), MASK=MM1, DIM=7, KIND=RR6(:,:,:,:,:,:,-1,:,:,:)%KIND-8 ) .NE. 0)) ERROR STOP 65
  IF (KIND(MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:), MASK=MM1, DIM=7, KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-8 )) .NE. 8)  ERROR STOP 66


  CC = ACHAR(1)
  CC(:,:,:,:,:,:,:,-1,:,:) = ACHAR(0)
  IF (ANY( MINLOC(ARRAY=CC, MASK=MM1, DIM=8, KIND=CC%KIND )  .NE. 0))   ERROR STOP 70
  IF (KIND(MINLOC(ARRAY=CC, MASK=MM1, DIM=8, KIND=CC%KIND )) .NE. 1)    ERROR STOP 71



  END
