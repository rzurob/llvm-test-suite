!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 08, 2006
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
!*  The target -- array section
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrTar
  IMPLICIT NONE

  TYPE :: DT
    SEQUENCE
    INTEGER      :: I
    CHARACTER(1) :: C
  END TYPE

  INTEGER               :: I

  TYPE(DT),     TARGET  :: Arr(100, 2)
  TYPE(DT),     POINTER :: Ptr(:,:)
  TYPE(DT)              :: Arr1(10,10)
  INTEGER,      POINTER :: IPtr(:,:)
  CHARACTER(1), POINTER :: CPtr(:,:)

  Arr(:, 1)%I = (/(I, I=1, 100)/)
  Arr(:, 1)%C = (/(CHAR(I), I=1, 100)/)
  Arr(:, 2)%I = (/(I, I=1, 100)/)
  Arr(:, 2)%C = (/(CHAR(I), I=1, 100)/)

  Ptr(0:, 0:) => Arr(::2, ::2)
  IF (.NOT. ASSOCIATED(Ptr, Arr(::2, ::2)))            STOP 11
  IF (ANY( LBOUND(Ptr) .NE. (/ 0, 0 /)))               STOP 12
  IF (ANY( UBOUND(Ptr) .NE. (/49, 0 /)))               STOP 13
  IF (ANY( Ptr(:,0)%I  .NE. (/(I,I=1,100,2 )/)))       STOP 14
  IF (ANY( Ptr(:,0)%C  .NE. (/(CHAR(I),I=1,100,2 )/))) STOP 15

  Ptr(0:9, 0:4) => Arr(::2, 2)
  Arr1 = RESHAPE(Arr(::2, :) , (/10,10/))
! IF (.NOT. ASSOCIATED(Ptr, Arr(::2, 2:2)))            STOP 21
  IF (.NOT. ASSOCIATED(Ptr))                           STOP 21
  IF (ANY( LBOUND(Ptr) .NE. (/ 0, 0 /)))               STOP 22
  IF (ANY( UBOUND(Ptr) .NE. (/ 9, 4 /)))               STOP 23
  IF (ANY( Ptr%I       .NE. Arr1%I))                   STOP 24
  IF (ANY( Ptr%C       .NE. Arr1%C))                   STOP 25

  I = -1
  Ptr(0:, 0:) => Arr(:, 0:I)
  IF ( ASSOCIATED(Ptr, Arr(:, 0:I)))                   STOP 31
  IF (ANY( LBOUND(Ptr) .NE. (/ 0, 1 /)))               STOP 32
  IF (ANY( SHAPE(Ptr)  .NE. (/100, 0 /)))              STOP 33

  I = -1
  Ptr(0:9, 0:I) => Arr(I, 0:I)
  IF ( ASSOCIATED(Ptr, Arr(0:I, 0:I)))                 STOP 41
  IF (ANY( LBOUND(Ptr) .NE. (/ 0, 1 /)))               STOP 42
  IF (ANY( SHAPE(Ptr)  .NE. (/ 10, 0 /)))              STOP 43


  END


