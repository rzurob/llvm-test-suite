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
!*  Result Characteristics.
!*  Integer. If KIND is present, the kind type parameter is that specified by the value of KIND;
!*  otherwise the kind type parameter is that of default integer type. If DIM is absent, the result
!*  is an array of rank one and of size equal to the rank of ARRAY; otherwise, the result is of rank
!*  n - 1 and shape (d1, d2, ..., dDIM-1, dDIM+1, ..., dn), where (d1, d2, ..., dn) is the shape of ARRAY.
!*
!*  (324281)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMinloc5
  IMPLICIT NONE

  INTEGER                :: I
  INTEGER(1), PARAMETER  :: II1(-1:126)  = (/( I, I=127, 0, -1) /)
  INTEGER(2), PARAMETER  :: II2(-1:126)  = (/( I, I=127, 0, -1) /)
  INTEGER(4), PARAMETER  :: II4(-1:126)  = (/( I, I=127, 0, -1) /)
  INTEGER(8), PARAMETER  :: II8(-1:126)  = (/( I, I=127, 0, -1) /)

  REAL(4),   PARAMETER  :: RR4(-1:126)  = (/( I, I=127, 0, -1) /)
  REAL(8),   PARAMETER  :: RR8(-1:126)  = (/( I, I=127, 0, -1) /)
  REAL(16),  PARAMETER  :: RR6(-1:126)  = (/( I, I=127, 0, -1) /)

  CHARACTER, PARAMETER  :: CC(-1:126)  = (/( ACHAR(I), I=127, 0, -1) /)
  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  LOGICAL      :: MM(-1:126)=.TRUE.
  REAL         :: R(-1:126, -1:126)= 1.1



  DO I1 = -1, 125
    IF (ANY( MINLOC(ARRAY=II1(I1:125), MASK=MM, KIND=II1%KIND ) .NE. 126-I1)) ERROR STOP 11
    IF (KIND(MINLOC(ARRAY=II1(I1:125), MASK=MM, KIND=II1%KIND ))  .NE. 1)     ERROR STOP 12
    IF (ANY( MINLOC(ARRAY=II2(I1:125), MASK=MM, KIND=II2%KIND ) .NE. 126-I1)) ERROR STOP 13
    IF (KIND(MINLOC(ARRAY=II2(I1:125), MASK=MM, KIND=II2%KIND )) .NE. 2)      ERROR STOP 14
    IF (ANY( MINLOC(ARRAY=II4(I1:125), MASK=MM, KIND=II4%KIND ) .NE. 126-I1)) ERROR STOP 15
    IF (KIND(MINLOC(ARRAY=II4(I1:125), MASK=MM, KIND=II4%KIND )) .NE. 4)      ERROR STOP 16
    IF (ANY( MINLOC(ARRAY=II8(I1:125), MASK=MM, KIND=II8%KIND ) .NE. 126-I1)) ERROR STOP 17
    IF (KIND(MINLOC(ARRAY=II8(I1:125), MASK=MM, KIND=II8%KIND )) .NE. 8)      ERROR STOP 18
  END DO

  DO I2 = -1, 126
    IF (ANY( MINLOC(ARRAY=RR4(:I2), MASK=.TRUE._8, KIND=RR4(:I2)%KIND )     .NE. I2+2)) ERROR STOP 21
    IF (KIND(MINLOC(ARRAY=RR4(:I2), MASK=.TRUE._8, KIND=RR4(:I2)%KIND ))    .NE. 4)     ERROR STOP 22
    IF (ANY( MINLOC(ARRAY=RR8(:I2), MASK=.TRUE._4, KIND=RR8(:I2)%KIND )     .NE. I2+2)) ERROR STOP 23
    IF (KIND(MINLOC(ARRAY=RR8(:I2), MASK=.TRUE._4, KIND=RR8(:I2)%KIND ))    .NE. 8)     ERROR STOP 24
    IF (ANY( MINLOC(ARRAY=RR6(:I2), MASK=.TRUE._2, KIND=RR6(:I2)%KIND-12 )  .NE. I2+2)) ERROR STOP 25
    IF (KIND(MINLOC(ARRAY=RR6(:I2), MASK=.TRUE._2, KIND=RR6(:I2)%KIND-12 )) .NE. 4)     ERROR STOP 26

  END DO

  DO I4 =-1, 126
    IF (ANY( MINLOC(ARRAY=CC(-1:I4), MASK=.FALSE._1, KIND=KIND(II1) ) .NE. 0)) ERROR STOP 31
    IF (KIND(MINLOC(ARRAY=CC(-1:I4), MASK=.FALSE._1, KIND=KIND(II1))) .NE. 1 ) ERROR STOP 32
    IF (ANY( MINLOC(ARRAY=CC(-1:I4), MASK=.FALSE._2, KIND=KIND(II2) ) .NE. 0)) ERROR STOP 33
    IF (KIND(MINLOC(ARRAY=CC(-1:I4), MASK=.FALSE._2, KIND=KIND(II2))) .NE. 2 ) ERROR STOP 34
    IF (ANY( MINLOC(ARRAY=CC(-1:I4), MASK=.FALSE._4, KIND=KIND(II4) ) .NE. 0)) ERROR STOP 35
    IF (KIND(MINLOC(ARRAY=CC(-1:I4), MASK=.FALSE._4, KIND=KIND(II4))) .NE. 4 ) ERROR STOP 36
    IF (ANY( MINLOC(ARRAY=CC(-1:I4), MASK=.FALSE._8, KIND=KIND(II8) ) .NE. 0)) ERROR STOP 37
    IF (KIND(MINLOC(ARRAY=CC(-1:I4), MASK=.FALSE._8, KIND=KIND(II8))) .NE. 8 ) ERROR STOP 38
  END DO




  END
