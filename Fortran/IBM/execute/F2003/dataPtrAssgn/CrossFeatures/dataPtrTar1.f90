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
!*  The target -- sub object
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrTar1
  IMPLICIT NONE

  TYPE :: DT
    SEQUENCE
    INTEGER      :: I
    CHARACTER(1) :: C
  END TYPE

  INTEGER               :: I

  TYPE(DT),     TARGET  :: Arr(100, 2)
  TYPE(DT),     POINTER :: Ptr(:,:)
  CHARACTER(1)          :: Arr1(10,10)
  INTEGER,      POINTER :: IPtr(:,:)
  CHARACTER(1), POINTER :: CPtr(:,:)

  Arr(:, 1)%I = (/(I, I=1, 100)/)
  Arr(:, 1)%C = (/(CHAR(I), I=1, 100)/)
  Arr(:, 2)%I = (/(I, I=1, 100)/)
  Arr(:, 2)%C = (/(CHAR(I), I=1, 100)/)

  IPtr(0:, 0:) => Arr(::2, ::2)%I
  IF (.NOT. ASSOCIATED(IPtr, Arr(::2, ::2)%I))       STOP 11
  IF (ANY( LBOUND(IPtr) .NE. (/ 0, 0 /)))            STOP 12
  IF (ANY( UBOUND(IPtr) .NE. (/49, 0 /)))            STOP 13
  IF (ANY( IPtr(:,0)  .NE. (/(I,I=1,100,2 )/)))      STOP 14

  CPtr(0:9, 0:4) => Arr(::2, 2)%C
  Arr1 = RESHAPE(Arr(::2, 2)%C , (/10,5/))
  !IF (.NOT. ASSOCIATED(CPtr, Arr(::2, 2:2)%C))      STOP 21
  IF (.NOT. ASSOCIATED(CPtr))                        STOP 21
  IF (ANY( LBOUND(CPtr) .NE. (/ 0, 0 /)))            STOP 22
  IF (ANY( UBOUND(CPtr) .NE. (/ 9, 4 /)))            STOP 23
  IF (ANY( CPtr        .NE. Arr1))                   STOP 25

  I = -1
  IPtr(0:, 0:) => Arr(:, 0:I)%I
  IF ( ASSOCIATED(IPtr, Arr(:, 0:I)%I))              STOP 31
  IF (ANY( LBOUND(IPtr) .NE. (/ 0, 1 /)))            STOP 32
  IF (ANY( UBOUND(IPtr)  .NE. (/99, 0 /)))           STOP 33

  I = -1
  CPtr(0:9, 0:I) => Arr(0:I, 1)%C
  IF ( ASSOCIATED(CPtr, Arr(0:I, 1:1)%C))            STOP 41
  IF (ANY( LBOUND(CPtr) .NE. (/ 0, 1 /)))            STOP 42
  IF (ANY( UBOUND(CPtr)  .NE. (/ 9, 0 /)))           STOP 43


  END


