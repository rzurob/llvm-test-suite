! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=self /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrAllocate.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*********************************************************************
!*  ===================================================================
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
!*  the allocate stmt
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND        :: K1
    INTEGER, LEN         :: N1
    INTEGER(K1), PRIVATE :: ID0=0
    INTEGER(K1)          :: ID
  END TYPE

  END MODULE


  PROGRAM dataPtrAllocate
  USE M
  IMPLICIT NONE

  TYPE(DT(20,4)), TARGET   :: Arr(10, 10), Arr1(100)
  CLASS(DT(:,4)), POINTER :: Ptr1(:, :)
  CLASS(*),  POINTER :: Ptr(:, :)
  INTEGER            :: I, J, K, N

  N = 10; K = 1
  Arr  = DT(20,4)(ID=-1)
  Arr1 = (/(DT(20,4)(ID=-I), I=1, 100)/)

  DO I =1, N
  DO J =1, N

    Ptr(I:, J:) => Arr(N:I:-K, N:I:-K)
    ALLOCATE(Ptr(I, J), SOURCE=-I)
    IF (.NOT. ASSOCIATED(Ptr))                 STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/1 , 1/)))      STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I,  J/)))      STOP 13
    SELECT TYPE (Ptr)
    TYPE IS(INTEGER)
        IF (ANY( Ptr     .NE. -I ))            STOP 14
    CLASS DEFAULT
      STOP 15
    END SELECT
    DEALLOCATE(Ptr)

  END DO
  END DO

  DO I =1, N
  DO J =I, N

    Ptr1(I:J, I:J) => Arr1(N*N::-K)
    ALLOCATE(Ptr(I:J, I:J), SOURCE=Ptr1)
    IF (.NOT. ASSOCIATED(Ptr))                 STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I , I/)))      STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      STOP 23
    SELECT TYPE (Ptr)
    TYPE IS(DT(*,4))
        IF (ANY( Ptr%ID  .NE. Ptr1(I:J, I:J)%ID ))   STOP 24
    CLASS DEFAULT
      STOP 15
    END SELECT
    DEALLOCATE(Ptr)

  END DO
  END DO


  END


