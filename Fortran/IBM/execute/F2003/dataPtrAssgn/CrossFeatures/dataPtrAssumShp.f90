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
!*  Assumed shape array
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrAssumShp
  IMPLICIT NONE

  REAL(8),      TARGET  :: RArr(10,10), R
  COMPLEX(8),   TARGET  :: CArr(10,10), C

  CLASS(*),     POINTER :: RPtr(:,:)
  CLASS(*),     POINTER :: CPtr(:,:)

  INTEGER           :: N, L, U

  R    = -1.0
  C    = (1.0, -1.0)
  RArr = R
  CArr = C
  N =10
  L = 0
  U = 9

  CALL S(RPtr, RArr, 0, 9)

  IF (.NOT. ASSOCIATED(RPtr, RArr))                 STOP 11
  IF (ANY( LBOUND(RPtr)         .NE. (/L, L /)))    STOP 12
  IF (ANY( UBOUND(RPtr)         .NE. (/U, U /)))    STOP 13
  SELECT TYPE (RPtr)
  TYPE IS (REAL(8))
    IF (ANY( RPtr               .NE.  R  ))         STOP 14
  CLASS DEFAULT
    STOP 15
  END SELECT

  CAll S1(RPtr, RArr, 0, 9)

  IF (.NOT. ASSOCIATED(RPtr, RArr(:,1:1)))          STOP 21
  IF (ANY( LBOUND(RPtr)         .NE. (/L, L /)))    STOP 22
  IF (ANY( UBOUND(RPtr)         .NE. (/U, L /)))    STOP 23
  SELECT TYPE (RPtr)
  TYPE IS (REAL(8))
    IF (ANY( RPtr               .NE.   R ))         STOP 24
  CLASS DEFAULT
    STOP 25
  END SELECT

  CALL S(CPtr, CArr, 0, 9)

  IF (.NOT. ASSOCIATED(CPtr, CArr))                 STOP 31
  IF (ANY( LBOUND(CPtr)         .NE. (/L, L /)))    STOP 32
  IF (ANY( UBOUND(CPtr)         .NE. (/U, U /)))    STOP 33
  SELECT TYPE (CPtr)
  TYPE IS (COMPLEX(8))
    IF (ANY( CPtr               .NE.  C    ))       STOP 34
  CLASS DEFAULT
    STOP 35
  END SELECT

  CAll S1(CPtr, CArr, 0, 9)

  IF (.NOT. ASSOCIATED(CPtr, CArr(:,1:1)))          STOP 41
  IF (ANY( LBOUND(CPtr)         .NE. (/L, L /)))    STOP 42
  IF (ANY( UBOUND(CPtr)         .NE. (/U, L /)))    STOP 43
  SELECT TYPE (CPtr)
  TYPE IS (COMPLEX(8))
    IF (ANY( CPtr               .NE. C    ))        STOP 44
  CLASS DEFAULT
    STOP 45
  END SELECT


  CONTAINS

  SUBROUTINE S(Ptr, Arr, L, U)
  CLASS(*), TARGET  :: Arr(L:, L:)
  INTEGER           :: L, U
  CLASS(*), POINTER :: Ptr(:, :)

  Ptr(L:, L:) => Arr

  END SUBROUTINE

  SUBROUTINE S1(Ptr, Arr, L, U)
  CLASS(*), TARGET  :: Arr(L:, L:)
  INTEGER           :: L, U
  CLASS(*), POINTER :: Ptr(:, :)

  Ptr(L:U, L:L) => Arr(:,L)

  END SUBROUTINE


  END


