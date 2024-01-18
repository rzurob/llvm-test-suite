!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrExpArr.f
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



  PROGRAM dataPtrExpArr
  IMPLICIT NONE

  INTEGER,      TARGET :: IArr(10,10)
  CHARACTER(3), TARGET :: CArr(10,10)

  IArr = -1
  CArr = "123"

  CALL S(IArr, 10, 0, 9)
  CAll S(CArr, 10, 0, 9)

  CONTAINS

  SUBROUTINE S(Arr, N, L, U)
  CLASS(*), TARGET  :: Arr(N, N)
  INTEGER           :: N, L, U
  CLASS(*), POINTER :: Ptr(:, :)


  N = 10000000 ! not affect Arr

  Ptr(L:, L:) => Arr
  IF (.NOT. ASSOCIATED(Ptr, Arr))                  STOP 11
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L /)))    STOP 12
  IF (ANY( UBOUND(Ptr)         .NE. (/U, U /)))    STOP 13
  SELECT TYPE (Ptr)
  TYPE IS (INTEGER)
    IF (ANY( Ptr               .NE.   -1))         STOP 14
  TYPE IS (CHARACTER(*))
    IF (ANY( Ptr               .NE.  "123"))       STOP 15
  CLASS DEFAULT
    STOP 16
  END SELECT

  Ptr(L:U, L:L) => Arr(:,2)
  IF (.NOT. ASSOCIATED(Ptr, Arr(:,2:2)))           STOP 21
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L /)))    STOP 22
  IF (ANY( UBOUND(Ptr)         .NE. (/U, L /)))    STOP 23
  SELECT TYPE (Ptr)
  TYPE IS (INTEGER)
    IF (ANY( Ptr               .NE.   -1))         STOP 24
  TYPE IS (CHARACTER(*))
    IF (ANY( Ptr               .NE.  "123"))       STOP 25
  CLASS DEFAULT
    STOP 16
  END SELECT


  END SUBROUTINE

  END


