!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 27, 2006
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
!*  - COUNT
!*  (318959/325220)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemCOUNT
  IMPLICIT NONE
  INTEGER :: I, J, K

  REAL(4),   PARAMETER :: B4(2,3)=RESHAPE((/1.,2.,3.,4.,5.,6./),(/2,3/))
  REAL(8),   PARAMETER :: B8(2,3)=B4
  REAL(16),  PARAMETER :: B16(2,3)=B8

  REAL(4),   PARAMETER :: C4(2,3)=RESHAPE((/0.,7.,3.,4.,5.,8./),(/2,3/))
  REAL(8),   PARAMETER :: C8(2,3)=C4
  REAL(16),  PARAMETER :: C16(2,3)=C8

  INTEGER,      PARAMETER :: R1(3) =RESHAPE((/2, 0, 1/),(/3/))
  INTEGER,      PARAMETER :: R3(2) =RESHAPE((/1, 2/),(/2/))
  INTEGER(1),   PARAMETER :: R21(2)=RESHAPE((/1, 2/),(/2/))
  INTEGER(2),   PARAMETER :: R22(2)=R21
  INTEGER(4),   PARAMETER :: R24(2)=R22
  INTEGER(8),   PARAMETER :: R28(2)=R24

  INTEGER(KIND(COUNT(R21 .EQ. 0))) :: T11=COUNT(R21    .EQ. R21)
  INTEGER(KIND(COUNT(R22 .EQ. 0))) :: T12=COUNT(R22(:) /=   R22(:))
  INTEGER(KIND(COUNT(R24 .EQ. 0))) :: T14=COUNT(R24(:) .EQ. R24)
  INTEGER(KIND(COUNT(R28 .EQ. 0))) :: T18=COUNT(R28    .NE. R21(1:))

  LOGICAL(1), PARAMETER   :: Z1(1:0, 2)= .TRUE.
  LOGICAL(2), PARAMETER   :: Z2(1:0, 2)= .TRUE.
  LOGICAL(4), PARAMETER   :: Z4(1:0, 2)= .TRUE.
  LOGICAL(8), PARAMETER   :: Z8(1:0, 2)= .TRUE.

  INTEGER(KIND(COUNT(Z1))), PARAMETER  :: T21(0)=COUNT(Z1)
  INTEGER(KIND(COUNT(Z2))), PARAMETER  :: T22(0)=COUNT(Z2)
  INTEGER(KIND(COUNT(Z4))), PARAMETER  :: T24(0)=COUNT(Z4)
  INTEGER(KIND(COUNT(Z8))), PARAMETER  :: T28(0)=COUNT(Z8)

  INTEGER(KIND(COUNT(T21 /= 0))), PARAMETER :: T31(3)=COUNT(LOGICAL(B4  .NE. C8, KIND=1), 1)
  INTEGER(KIND(COUNT(T22 /= 0))), PARAMETER :: T32(3)=COUNT(LOGICAL(B16 .NE. C4, KIND=2), 1)
  INTEGER(KIND(COUNT(T24 /= 0))), PARAMETER :: T34(3)=COUNT((B4 .NE. C4), 1)
  INTEGER(KIND(COUNT(T28 /= 0))), PARAMETER :: T38(3)=COUNT((B8 .NE. C4), 1)

  INTEGER(KIND(COUNT(T31 /= 0, KIND=1))), PARAMETER :: T41(2)=COUNT(LOGICAL(B4 .NE. C8,  KIND=1), DIM=2)
  INTEGER(KIND(COUNT(T32 /= 0, KIND=2))), PARAMETER :: T42(2)=COUNT(LOGICAL(B8 .NE. C16, KIND=2), DIM=2)
  INTEGER(KIND(COUNT(T34 /= 0, KIND=4))), PARAMETER :: T44(2)=COUNT((B4 .NE. C4(:,  1:)),DIM=2)
  INTEGER(KIND(COUNT(T38 /= 0, KIND=8))), PARAMETER :: T48(2)=COUNT((B4 .NE. C8(1:, :)), DIM=2)

  INTEGER(KIND(COUNT(T41 /= 0, KIND=1))) :: T51(2)=COUNT(LOGICAL(B4 .NE. C8,  KIND=8), DIM=2, KIND=1)
  INTEGER(KIND(COUNT(T42 /= 0, KIND=2))) :: T52(2)=COUNT(LOGICAL(B8 .NE. C16, KIND=4), DIM=2, KIND=2)
  INTEGER(KIND(COUNT(T44 /= 0, KIND=4))) :: T54(2)=COUNT((B4 .NE. C4(:,  1:)),DIM=2, KIND=4)
  INTEGER(KIND(COUNT(T48 /= 0, KIND=8))) :: T58(2)=COUNT((B4 .NE. C8(1:, :)), DIM=2, KIND=8)


  IF (KIND(T11)   .NE.   4 )        ERROR STOP 11
  IF (     T11    .NE.   2 )        ERROR STOP 12
  IF (KIND(T12)   .NE.   4 )        ERROR STOP 13
  IF (     T12    .NE.   0 )        ERROR STOP 14
  IF (KIND(T14)   .NE.   4 )        ERROR STOP 15
  IF (     T14    .NE.   2)         ERROR STOP 16
  IF (KIND(T18)   .NE.   4 )        ERROR STOP 17
  IF (     T18    .NE.   0)         ERROR STOP 18


  IF (KIND(T21)   .NE.   4 )        ERROR STOP 21
  IF (KIND(T22)   .NE.   4 )        ERROR STOP 23
  IF (KIND(T24)   .NE.   4 )        ERROR STOP 25
  IF (KIND(T28)   .NE.   4 )        ERROR STOP 27


  IF ( KIND( T31) .NE.   4 )        ERROR STOP 31
  IF ( ANY(  T31  .NE.  R1))        ERROR STOP 32
  IF ( KIND( T32) .NE.   4 )        ERROR STOP 33
  IF ( ANY(  T32  .NE. R1) )        ERROR STOP 34
  IF ( KIND( T34) .NE.   4 )        ERROR STOP 35
  IF ( ANY(  T34  .NE. R1) )        ERROR STOP 36
  IF ( KIND( T38) .NE.   4 )        ERROR STOP 37
  IF ( ANY(  T38  .NE. R1) )        ERROR STOP 38

  IF ( KIND( T41) .NE.   1 )        ERROR STOP 41
  IF ( ANY(  T41  .NE. R3) )        ERROR STOP 42
  IF ( KIND( T42) .NE.   2 )        ERROR STOP 43
  IF ( ANY(  T42  .NE. R3) )        ERROR STOP 44
  IF ( KIND( T44) .NE.   4 )        ERROR STOP 45
  IF ( ANY(  T44  .NE. R3) )        ERROR STOP 46
  IF ( KIND( T48) .NE.   8 )        ERROR STOP 47
  IF ( ANY(  T48  .NE. R3) )        ERROR STOP 48

  IF ( KIND( T51) .NE.   1 )        ERROR STOP 51
  IF ( ANY(  T51  .NE. R3) )        ERROR STOP 52
  IF ( KIND( T52) .NE.   2 )        ERROR STOP 53
  IF ( ANY(  T52  .NE. R3) )        ERROR STOP 54
  IF ( KIND( T54) .NE.   4 )        ERROR STOP 55
  IF ( ANY(  T54  .NE. R3) )        ERROR STOP 56
  IF ( KIND( T58) .NE.   8 )        ERROR STOP 57
  IF ( ANY(  T58  .NE. R3) )        ERROR STOP 58



  END



