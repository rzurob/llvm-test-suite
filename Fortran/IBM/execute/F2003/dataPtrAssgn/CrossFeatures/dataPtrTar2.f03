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
!*  The target -- function return
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    INTEGER      :: I
    CHARACTER(1) :: C
  END TYPE

  INTERFACE
    FUNCTION ExtF1(Arg)
    IMPORT DT
      TYPE (DT), TARGET  :: Arg(:, :)
      CLASS(DT), POINTER :: ExtF1(:, :)
    END FUNCTION

    FUNCTION ExtF2(Arg)
    IMPORT DT
      TYPE (DT), TARGET  :: Arg(:)
      CLASS(DT), POINTER :: ExtF2(:)
    END FUNCTION
  END INTERFACE

  END MODULE

  FUNCTION ExtF1(Arg)
  USE M, ONLY :DT
  TYPE (DT), TARGET  :: Arg(:, :)
  CLASS(DT), POINTER :: ExtF1(:, :)
    ExtF1(LBOUND(Arg, 1):, LBOUND(Arg, 2):) => Arg
  END FUNCTION

  FUNCTION ExtF2(Arg)
  USE M, ONLY :DT
  TYPE (DT), TARGET  :: Arg(:)
  CLASS(DT), POINTER :: ExtF2(:)
    ExtF2(LBOUND(Arg, 1):UBOUND(Arg,1)) => Arg
  END FUNCTION

  PROGRAM dataPtrTar2
  USE M
  IMPLICIT NONE


  INTEGER               :: I

  TYPE(DT),     TARGET  :: Arr(100, 2)
  TYPE(DT),     POINTER :: Ptr(:,:)
  TYPE(DT)              :: Arr1(10,10)

  Arr(:, 1)%I = (/(I, I=1, 100)/)
  Arr(:, 1)%C = (/(CHAR(I), I=1, 100)/)
  Arr(:, 2)%I = (/(I, I=1, 100)/)
  Arr(:, 2)%C = (/(CHAR(I), I=1, 100)/)

  Ptr(0:, 0:) => ExtF1(Arr(::2, ::2))
  IF (.NOT. ASSOCIATED(Ptr, Arr(::2, ::2)))            ERROR STOP 11
  IF (ANY( LBOUND(Ptr) .NE. (/ 0, 0 /)))               ERROR STOP 12
  IF (ANY( UBOUND(Ptr) .NE. (/49, 0 /)))               ERROR STOP 13
  IF (ANY( Ptr(:,0)%I  .NE. (/(I,I=1,100,2 )/)))       ERROR STOP 14
  IF (ANY( Ptr(:,0)%C  .NE. (/(CHAR(I),I=1,100,2 )/))) ERROR STOP 15

  Ptr(0:9, 0:9) => ExtF2(Arr(::1, 2))
  Arr1 = RESHAPE(Arr(::1, 2) , (/10,10/))
  !IF (.NOT. ASSOCIATED(Ptr, Arr(::2, 2:2)))           ERROR STOP 21
  IF (.NOT. ASSOCIATED(Ptr))                           ERROR STOP 21
  IF (ANY( LBOUND(Ptr) .NE. (/ 0, 0 /)))               ERROR STOP 22
  IF (ANY( UBOUND(Ptr) .NE. (/ 9, 9 /)))               ERROR STOP 23
  IF (ANY( Ptr%I       .NE. Arr1%I))                   ERROR STOP 24
  IF (ANY( Ptr%C       .NE. Arr1%C))                   ERROR STOP 25

  I = -1
  Ptr(0:, 0:) => ExtF1(Arr(:, 0:I))
  IF ( ASSOCIATED(Ptr, Arr(:, 0:I)))                   ERROR STOP 31
  IF (ANY( LBOUND(Ptr) .NE. (/ 0, 1 /)))               ERROR STOP 32
  IF (ANY( UBOUND(Ptr) .NE. (/99, 0 /)))               ERROR STOP 33

  I = -1
  Ptr(0:I, 0:0) => ExtF2(Arr(0:I, 1))
  IF ( ASSOCIATED(Ptr, Arr(0:I, 1:1)))                 ERROR STOP 41
  IF (ANY( LBOUND(Ptr) .NE. (/ 1, 0 /)))               ERROR STOP 42
  IF (ANY( SHAPE(Ptr)  .NE. (/ 0, 1 /)))               ERROR STOP 43


  END

