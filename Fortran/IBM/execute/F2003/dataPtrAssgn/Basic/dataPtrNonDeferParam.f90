!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrNonDeferParam.f
!*
!*  DATE                       : Feb. 06, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  If data-target is an associated pointer, all nondeferred type parameters of the
!*  declared type of data-pointer-object shall have the same values
!*  s the corresponding type parameters of data-target.
!*
!*  (not include type paramter for derived type)
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrNonDeferParam
  IMPLICIT NONE

  INTEGER          :: I, J
  REAL(8), POINTER :: PtrReal8(:, :)
  REAL(8), POINTER :: Real8(:,:)

  CHARACTER(4), POINTER :: PtrC4(:,:)
  CHARACTER(:), POINTER :: PtrC4Tar(:, :)

  CHARACTER(:), POINTER :: PtrC3(:,:)
  CHARACTER(:), POINTER :: PtrC3Tar(:, :)

  CHARACTER(2), POINTER :: PtrC2(:,:)
  CHARACTER(2), POINTER :: PtrC2Tar(:, :)

  CHARACTER(:), POINTER :: PtrC1(:,:)
  CHARACTER(1), POINTER :: PtrC1Tar(:, :)

  ALLOCATE(Real8(1:1023,2))
  Real8(:, 1) = (/(I, I=1, 1023)/)
  Real8(:, 2) = (/(I, I=1, 1023)/)

  PtrReal8(0:, 0:) => Real8
  IF (ANY( LBOUND(PtrReal8) .NE. (/0,    0/))) STOP 11
  IF (ANY( UBOUND(PtrReal8) .NE. (/1022, 1/))) STOP 12
  IF (ANY(PtrReal8(:, 0)    .NE. (/(I, I=1, 1023)/)))  STOP 13
  IF (ANY(PtrReal8(:, 1)    .NE. (/(I, I=1, 1023)/)))  STOP 14

  PtrReal8(2:1024, 0:0) => Real8(:, 2)
  IF (ANY( LBOUND(PtrReal8) .NE. (/2,    0/))) STOP 21
  IF (ANY( UBOUND(PtrReal8) .NE. (/1024, 0/))) STOP 22
  IF (ANY(PtrReal8(:, 0)    .NE. (/(I, I=1, 1023)/)))  STOP 23

  DEALLOCATE(Real8)
  ALLOCATE(PtrC4Tar(1:1023, 1:2), SOURCE="1234")

  PtrC4(0:, 0:) => PtrC4Tar
  IF (ANY( LBOUND(PtrC4) .NE. (/0,    0/))) STOP 31
  IF (ANY( UBOUND(PtrC4) .NE. (/1022, 1/))) STOP 32
  IF (ANY(PtrC4(:, 0)    .NE. (/("1234", I=1, 1023)/)))  STOP 33
  IF (ANY(PtrC4(:, 1)    .NE. (/("1234", I=1, 1023)/)))  STOP 34

  PtrC4Tar(1:512, 1)    = (/("1234", I=1, 512)/)
  PtrC4Tar(513:1023, 1) = (/("4321", I=513, 1023)/)
  PtrC4(0:514, 0:0) => PtrC4Tar(:, 1)
  IF (ANY( LBOUND(PtrC4) .NE. (/0,    0/))) STOP 41
  IF (ANY( UBOUND(PtrC4) .NE. (/514,  0/))) STOP 42
  IF (ANY(PtrC4(:, 0)    .NE. (/("1234", I=1, 512), "4321", "4321", "4321"/)))  STOP 43

  DEALLOCATE( PtrC4Tar )

  ALLOCATE(PtrC3Tar(1:1023, 1:1), SOURCE="123")

  PtrC3(0:, 0:) => PtrC3Tar
  IF (ANY( LBOUND(PtrC3) .NE. (/0,    0/))) STOP 51
  IF (ANY( UBOUND(PtrC3) .NE. (/1022, 0/))) STOP 52
  IF (ANY(PtrC3(:, 0)    .NE. (/("123", I=1, 1023)/)))  STOP 53

  PtrC3Tar(:, 1) = (/("321", I=1, 1023)/)
  PtrC3(1:1021, 0:0) => PtrC3Tar(:, 1)
  IF (ANY( LBOUND(PtrC3) .NE. (/1,    0/))) STOP 61
  IF (ANY( UBOUND(PtrC3) .NE. (/1021, 0/))) STOP 62
  IF (ANY(PtrC3(:, 0)    .NE. (/("321", I=1, 1021)/)))  STOP 63

  DEALLOCATE( PtrC3Tar )

  ALLOCATE(PtrC2Tar(1:1023, 1:2))
  PtrC2Tar(:,1) = (/("12", I=1, 1023 )/)
  PtrC2Tar(:,2) = (/("21", I=1, 1023 )/)
  PtrC2(0:, 0:) => PtrC2Tar
  IF (ANY( LBOUND(PtrC2) .NE. (/0,    0/))) STOP 71
  IF (ANY( UBOUND(PtrC2) .NE. (/1022, 1/))) STOP 72
  IF (ANY(PtrC2(:, 0)    .NE. (/("12", I=1, 1023)/)))  STOP 73
  IF (ANY(PtrC2(:, 1)    .NE. (/("21", I=1, 1023)/)))  STOP 74

  PtrC2(1:1020, 0:0) => PtrC2Tar(:,1)
  IF (ANY( LBOUND(PtrC2) .NE. (/1,    0/))) STOP 81
  IF (ANY( UBOUND(PtrC2) .NE. (/1020, 0/))) STOP 82
  IF (ANY(PtrC2(:, 0)    .NE. (/("12", I=1, 1020)/)))  STOP 83
  DEALLOCATE(PtrC2Tar)

  ALLOCATE(PtrC1Tar(1:1023, 1:2))
  PtrC1Tar(:,1) = (/("1", I=1, 1023) /)
  PtrC1Tar(:,2) = (/("2", I=1, 1023) /)
  PtrC1(0:, 0:) => PtrC1Tar
  IF (ANY( LBOUND(PtrC1) .NE. (/0,    0/))) STOP 91
  IF (ANY( UBOUND(PtrC1) .NE. (/1022, 1/))) STOP 92
  IF (ANY(PtrC1(:, 0)    .NE. (/("1", I=1, 1023)/)))  STOP 93
  IF (ANY(PtrC1(:, 1)    .NE. (/("2", I=1, 1023)/)))  STOP 94

  ptrC1(10:1025, 0:0) => PtrC1Tar(:, 1)
  IF (ANY( LBOUND(PtrC1) .NE. (/10,   0/))) STOP 111
  IF (ANY( UBOUND(PtrC1) .NE. (/1025, 0/))) STOP 112
  IF (ANY(PtrC1(:, 0)    .NE. (/("1", I=1, 1016)/)))  STOP 113
  DEALLOCATE(PtrC1Tar)

  END


