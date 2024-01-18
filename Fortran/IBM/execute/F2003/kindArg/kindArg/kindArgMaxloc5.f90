!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 27, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : MAXLOC
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
!*  is an array of rank one and of size equal to the rank of ARRAY; otherwise, the result is of rank n - 1
!*  and shape (d1, d2, ..., dDIM-1, dDIM+1, ..., dn), where (d1, d2, ..., dn) is the shape of ARRAY.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMaxloc5
  IMPLICIT NONE

  INTEGER                :: I
  INTEGER(1), PARAMETER  :: II1(128)  = (/( I, I=0,127) /)
  INTEGER(2), PARAMETER  :: II2(128)  = (/( I, I=0,127) /)
  INTEGER(4), PARAMETER  :: II4(128)  = (/( I, I=0,127) /)
  INTEGER(8), PARAMETER  :: II8(128)  = (/( I, I=0,127) /)

  REAL(4),   PARAMETER  :: RR4(128)  = (/( I, I=0,127) /)
  REAL(8),   PARAMETER  :: RR8(128)  = (/( I, I=0,127) /)
  REAL(16),  PARAMETER  :: RR6(128)  = (/( I, I=0,127) /)

  CHARACTER, PARAMETER  :: CC(128)  = (/( ACHAR(I), I=0,127) /)
  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  LOGICAL      :: MM(128)=.TRUE.
  REAL         :: R(128,128)= 1.1



  DO I1 = 1, 127
    IF (ANY( MAXLOC(ARRAY=II1(I1:127), MASK=MM, KIND=II1%KIND )  .NE. 128-I1)) STOP 11
    IF (KIND(MAXLOC(ARRAY=II1(I1:127), MASK=MM, KIND=II1%KIND )) .NE. 1 )      STOP 12
    IF (ANY( MAXLOC(ARRAY=II2(I1:127), MASK=MM, KIND=II2%KIND )  .NE. 128-I1)) STOP 13
    IF (KIND(MAXLOC(ARRAY=II2(I1:127), MASK=MM, KIND=II2%KIND )) .NE. 2 )      STOP 14
    IF (ANY( MAXLOC(ARRAY=II4(I1:127), MASK=MM, KIND=II4%KIND )  .NE. 128-I1)) STOP 15
    IF (KIND(MAXLOC(ARRAY=II4(I1:127), MASK=MM, KIND=II4%KIND )) .NE. 4 )      STOP 16
    IF (ANY( MAXLOC(ARRAY=II8(I1:127), MASK=MM, KIND=II8%KIND )  .NE. 128-I1)) STOP 17
    IF (KIND(MAXLOC(ARRAY=II8(I1:127), MASK=MM, KIND=II8%KIND )) .NE. 8 )      STOP 18
  END DO

  DO I2 = 1, 128
    IF (ANY( MAXLOC(ARRAY=RR4(:I2), MASK=.TRUE._8, KIND=RR4(:I2)%KIND )  .NE. I2)) STOP 21
    IF (KIND(MAXLOC(ARRAY=RR4(:I2), MASK=.TRUE._8, KIND=RR4(:I2)%KIND )) .NE. 4 )  STOP 22
    IF (ANY( MAXLOC(ARRAY=RR8(:I2), MASK=.TRUE._4, KIND=RR8(:I2)%KIND )  .NE. I2)) STOP 23
    IF (KIND(MAXLOC(ARRAY=RR8(:I2), MASK=.TRUE._4, KIND=RR8(:I2)%KIND )) .NE. 8 )  STOP 24
    IF (ANY( MAXLOC(ARRAY=RR6(:I2), MASK=.TRUE._2, KIND=RR6(:I2)%KIND-8) .NE. I2)) STOP 25
    IF (KIND(MAXLOC(ARRAY=RR6(:I2), MASK=.TRUE._2, KIND=RR6(:I2)%KIND-8)).NE. 8  ) STOP 26

  END DO

  DO I4 =1, 128
    IF (ANY( MAXLOC(ARRAY=CC(1:I4), MASK=.FALSE._1, KIND=KIND(II1) ) .NE. 0)) STOP 31
    IF (KIND(MAXLOC(ARRAY=CC(1:I4), MASK=.FALSE._1, KIND=KIND(II1))) .NE. 1)  STOP 32
    IF (ANY( MAXLOC(ARRAY=CC(1:I4), MASK=.FALSE._2, KIND=KIND(II2) ) .NE. 0)) STOP 33
    IF (KIND(MAXLOC(ARRAY=CC(1:I4), MASK=.FALSE._2, KIND=KIND(II2))) .NE. 2)  STOP 34
    IF (ANY( MAXLOC(ARRAY=CC(1:I4), MASK=.FALSE._4, KIND=KIND(II4) ) .NE. 0)) STOP 35
    IF (KIND(MAXLOC(ARRAY=CC(1:I4), MASK=.FALSE._4, KIND=KIND(II4))) .NE. 4)  STOP 36
    IF (ANY( MAXLOC(ARRAY=CC(1:I4), MASK=.FALSE._8, KIND=KIND(II8) ) .NE. 0)) STOP 37
    IF (KIND(MAXLOC(ARRAY=CC(1:I4), MASK=.FALSE._8, KIND=KIND(II8))) .NE. 8)  STOP 38
  END DO




  END

