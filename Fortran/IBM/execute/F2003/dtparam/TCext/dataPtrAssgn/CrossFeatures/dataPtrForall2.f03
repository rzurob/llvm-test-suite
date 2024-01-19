! GB DTP extension using:
! ftcx_dtp -qck -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrForall2.f
! opt variations: -qnock -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 16, 2006
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
!*  Forall
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1,N1,K2)    ! (1,3,4)
    INTEGER, KIND                      :: K1,K2
    INTEGER, LEN                       :: N1
    CHARACTER(kind=K1,len=N1), PRIVATE :: C="???"
    INTEGER(K2)                        :: ID
    CLASS(DT(K1,:,K2)), POINTER           :: Ptr(:, :)=>NULL()
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (1,3,4)
    CHARACTER(kind=K1,len=N1), PRIVATE :: CC="???"
  END TYPE

  END MODULE

  PROGRAM dataPtrForall2
  USE M
  IMPLICIT NONE

  TYPE(DT(1,3,4)), TARGET   :: Tar2(10, 10)
  TYPE(DT1(1,3,4)), TARGET  :: Tar1(100)
  CLASS(DT(1,:,4)), POINTER :: T(:, :)
  LOGICAL            :: L(10, 10)
  INTEGER            :: I, J, K, N

  N = 10; K = 0
  ALLOCATE(DT(1,3,4) :: T(N,N))

  FORALL (I=1:N, J=1:N)
    T(I,J)%Ptr(I:, J:) => Tar2
    L(I, J) = Check1(T(I,J)%Ptr, Tar2, I, J)
  END FORALL

  DO I = 1, N
  DO J = 1, N
    IF (.NOT. L(I, J) ) THEN
      PRINT *, I, J
      STOP 11
    END IF
  END DO
  END DO

  L = .TRUE.

  FORALL (I=1:N, J=I:N)
    T(I,J)%Ptr(I:J, I:J) => Tar1
    L(I, J) = Check2(T(I,J)%Ptr, Tar1, I, J)
  END FORALL


  DO I = 1, N
  DO J = I, N
    IF (.NOT. L(I, J) ) THEN
      PRINT *, I, J
      STOP 12
    END IF
  END DO
  END DO


  CONTAINS

  PURE FUNCTION Check1(Ptr, Arr, I, J)
  TYPE(DT(1,*,4)),  TARGET,  INTENT(IN) :: Arr(:, :)
  CLASS(DT(1,:,4)), POINTER, INTENT(IN) :: Ptr(:, :)
  INTEGER,            INTENT(IN) :: I, J
  LOGICAL                        :: Check1

    Check1 = .TRUE.

    SELECT TYPE(Ptr)
    TYPE IS (DT(1,*,4))
 !    Ptr = DT(ID=I*J, Ptr=NULL()) ! violate C1272
    CLASS DEFAULT
 !    Check1 = .FALSE.
    END SELECT

 !  IF (.NOT. ASSOCIATED(Ptr, Tar2))             Check1 = .FALSE.
 !  IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        Check1 = .FALSE.
 !  IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) Check1 = .FALSE.

  END FUNCTION

  PURE FUNCTION Check2(Ptr, Arr, I, J)
  TYPE(DT1(1,*,4)), TARGET,  INTENT(IN) :: Arr(:)
  CLASS(DT(1,:,4)), POINTER, INTENT(IN) :: Ptr(:, :)
  INTEGER,            INTENT(IN) :: I, J
  LOGICAL                        :: Check2

    Check2 = .TRUE.

    SELECT TYPE (Ptr)
    TYPE IS (DT1(1,*,4))
 !    Ptr = DT1(ID=-I*J, Ptr=NULL())  ! violate C1272
    CLASS DEFAULT
      Check2 = .FALSE.
    END SELECT

 !  IF (.NOT. ASSOCIATED(Ptr))                 Check2 = .FALSE.
 !  IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      Check2 = .FALSE.
 !  IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      Check2 = .FALSE.

  END FUNCTION

  END


