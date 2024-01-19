!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 20, 2006
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
!*  Function return
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrFuncRet
  IMPLICIT NONE

  INTEGER,  TARGET  :: Tar2(100, 100)
  INTEGER,  TARGET  :: Tar1(10000)
  CLASS(*), POINTER  :: Ptr(:, :)
  INTEGER    :: I, J, K, N

  INTERFACE
    FUNCTION  ExtFun1(Arr, I, J, N)
      INTEGER  :: I, J, N
      CLASS(*), POINTER :: ExtFun1(:, :)
      INTEGER, TARGET  :: Arr(N*N)
    END FUNCTION

    FUNCTION ExtFun2(Arr, I, J, N)
      INTEGER  :: I, J, N
      CLASS(*), POINTER :: ExtFun2(:, :)
      INTEGER, TARGET  :: Arr(N, N)
    END FUNCTION
  END INTERFACE


  N = 100; K = 0
  Tar1 = -1
  Tar2 = -2

  DO I =1, N
  DO J =I, N

    Ptr => ExtFun2(Tar2(1,1), I, J, N)
    IF (.NOT. ASSOCIATED(Ptr, Tar2))             ERROR STOP 11

    SELECT TYPE( Ptr )
    TYPE IS (INTEGER)
      Ptr = I*J
      IF (SIZE(Ptr)  .NE. N*N )                    ERROR STOP 10
      IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        ERROR STOP 12
      IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) ERROR STOP 13
      IF (ANY( Tar2     .NE.  I*J ))               ERROR STOP 14
    CLASS DEFAULT
      STOP 15
    END SELECT

    Ptr => ExtFun1(Tar1(1), I, J, N)
    IF (.NOT. ASSOCIATED(Ptr))                      ERROR STOP 21

    SELECT TYPE( Ptr )
    TYPE IS (INTEGER)
      Ptr = -I*J
      IF (SIZE(Ptr)  .NE. (J-I+1)*(J-I+1))            ERROR STOP 20
      IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))           ERROR STOP 22
      IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))           ERROR STOP 23
      IF (ANY( Tar1(1:(J-I+1)*(J-I+1)) .NE.  -I*J ))  ERROR STOP 24
    CLASS DEFAULT
      STOP 15
    END SELECT

  END DO
  END DO

  END

  FUNCTION  ExtFun1(Arr, I, J, N)
  INTEGER  :: I, J, N
  CLASS(*), POINTER :: ExtFun1(:, :)
  INTEGER, TARGET  :: Arr(N*N)
    ExtFun1(I:J, I:J) => Arr
  END FUNCTION

  FUNCTION ExtFun2(Arr, I, J, N)
  INTEGER  :: I, J, N
  CLASS(*), POINTER :: ExtFun2(:, :)
  INTEGER, TARGET  :: Arr(N, N)
    ExtFun2(I:, J:) => Arr
  END FUNCTION


