!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 09, 2006
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
!*  Explicit shape array
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrExpArr1
  IMPLICIT NONE

  INTEGER,      TARGET  :: IArr(0:9,0:9)
  CHARACTER(3), TARGET  :: CArr(0:9,0:9)

  CLASS(*),     POINTER :: IPtr(:,:)
  CLASS(*),     POINTER :: CPtr(:,:)

  INTEGER           :: N, L, U

  IArr = -1
  CArr = "123"
  N =10
  L = 0
  U = 9

  CALL S(IPtr, IArr, N, L, U)

  IF (.NOT. ASSOCIATED(IPtr, IArr))                 ERROR STOP 11
  IF (ANY( LBOUND(IPtr)         .NE. (/L, L /)))    ERROR STOP 12
  IF (ANY( UBOUND(IPtr)         .NE. (/U, U /)))    ERROR STOP 13
  SELECT TYPE (IPtr)
  TYPE IS (INTEGER)
    IF (ANY( IPtr               .NE.   -1))         ERROR STOP 14
  CLASS DEFAULT
    STOP 15
  END SELECT

  CAll S1(IPtr, IArr,  N, L, U)
  !IF (.NOT. ASSOCIATED(IPtr, IArr(L:U,L:L)))       ERROR STOP 21
  IF (.NOT. ASSOCIATED(IPtr))                       ERROR STOP 21
  IF (ANY( LBOUND(IPtr)         .NE. (/L, L /)))    ERROR STOP 22
  IF (ANY( UBOUND(IPtr)         .NE. (/U, L /)))    ERROR STOP 23
  SELECT TYPE (IPtr)
  TYPE IS (INTEGER)
    IF (ANY( IPtr               .NE.   -1))         ERROR STOP 24
  CLASS DEFAULT
    STOP 25
  END SELECT

  CALL S(CPtr, CArr, N, L, U)

  IF (.NOT. ASSOCIATED(CPtr, CArr))                 ERROR STOP 31
  IF (ANY( LBOUND(CPtr)         .NE. (/L, L /)))    ERROR STOP 32
  IF (ANY( UBOUND(CPtr)         .NE. (/U, U /)))    ERROR STOP 33
  SELECT TYPE (CPtr)
  TYPE IS (CHARACTER(*))
    IF (ANY( CPtr               .NE.  "123"))       ERROR STOP 34
  CLASS DEFAULT
    STOP 35
  END SELECT

  CAll S1(CPtr, CArr, N, L, U)

  IF (.NOT. ASSOCIATED(CPtr, CArr(:,L:L)))          ERROR STOP 41
  IF (ANY( LBOUND(CPtr)         .NE. (/L, L /)))    ERROR STOP 42
  IF (ANY( UBOUND(CPtr)         .NE. (/U, L /)))    ERROR STOP 43
  SELECT TYPE (CPtr)
  TYPE IS (CHARACTER(*))
    IF (ANY( CPtr               .NE. "123"))        ERROR STOP 44
  CLASS DEFAULT
    STOP 45
  END SELECT


  CONTAINS

  SUBROUTINE S(Ptr, Arr, N, L, U)
  CLASS(*), TARGET  :: Arr(L:U,L:U)
  INTEGER           :: N, L, U
  CLASS(*), POINTER :: Ptr(:, :)

  N = 10000000 ! not affect Arr

  Ptr(L:, L:) => Arr

  END SUBROUTINE

  SUBROUTINE S1(Ptr, Arr, N, L, U)
  CLASS(*), TARGET  :: Arr(L:U,L:U)
  INTEGER           :: N, L, U
  CLASS(*), POINTER :: Ptr(:, :)

  N = 10000000 ! not affect Arr

  Ptr(L:U, L:L) => Arr(:,L)
  SELECT TYPE (Ptr)
  TYPE IS (INTEGER)
  TYPE IS (CHARACTER(*))
  CLASS DEFAULT
    STOP 55
  END SELECT

  END SUBROUTINE

  END

