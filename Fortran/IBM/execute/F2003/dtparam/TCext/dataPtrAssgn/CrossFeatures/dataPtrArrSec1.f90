! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrArrSec1.f
! opt variations: -ql -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrArrSec1.f
!*
!*  DATE                       : Feb. 15, 2006
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
!*  the array section
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1,K2)    ! (4,4)
    INTEGER, KIND        :: K1,K2
    INTEGER(K1), PRIVATE :: ID0=0
    INTEGER(K2)          :: ID
  END TYPE

  END MODULE


  PROGRAM dataPtrArrSec1
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Arr(:, :), Arr1(:)
  CLASS(*), POINTER :: Ptr(:, :), Ptr1(:)
  INTEGER            :: I, J, K, N

  N = 100
  ALLOCATE(Arr(N,N), SOURCE=DT(4,4)(ID=-1))
  ALLOCATE(Arr1(N*N), SOURCE=DT(4,4)(ID=-2))

  SELECT TYPE (Arr)
  TYPE IS (DT(4,4))
    Arr%ID  = RESHAPE((/(i, i=1, 10000)/), (/100, 100/))
  END SELECT

  SELECT TYPE (Arr1)
  TYPE IS (DT(4,4))
    Arr1%ID = (/(i, i=1, 10000)/)
  END SELECT

  DO I =1, N/3
  DO J =I, N/3
  DO K =1, 3

    Ptr => Arr
    Ptr(I:, J:) => Ptr(N:I:-K, N:J:-K)
    IF (.NOT. ASSOCIATED(Ptr,  Arr(N:I:-K, N:J:-K) ))      STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I , J/)))                  STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/(N-I)/K+I, (N-J)/K+J/)))      STOP 13

    SELECT TYPE (Ptr)
    TYPE IS(DT(4,4))
      SELECT TYPE( Arr)
      TYPE IS (DT(4,4))
        IF (ANY( Ptr%ID      .NE. Arr(N:I:-K, N:J:-K)%ID ))    STOP 14
      END SELECT
    CLASS DEFAULT
      STOP 15
    END SELECT

    Ptr1 => Arr1
    Ptr(I:J, I:J) => Ptr1(N*N::-K)
    IF (.NOT. ASSOCIATED(Ptr))                   STOP 20
    IF (SIZE(Ptr)         .NE. (J-I+1)*(J-I+1))  STOP 21
    IF (ANY( LBOUND(Ptr)  .NE. (/I , I /)))      STOP 22
    IF (ANY( UBOUND(Ptr)  .NE. (/J , J /)))      STOP 23

    SELECT TYPE (Ptr)
    TYPE IS(DT(4,4))
      SELECT TYPE (Arr1)
      TYPE IS(DT(4,4))
        IF (ANY( Ptr%ID  .NE. RESHAPE(Arr1(N*N::-K)%ID, (/J-I+1, J-I+1/)) ) ) STOP 24
      END SELECT
    CLASS DEFAULT
      STOP 25
    END SELECT

  END DO
  END DO
  END DO


  END


