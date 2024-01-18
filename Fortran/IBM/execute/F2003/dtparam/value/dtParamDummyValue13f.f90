!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!* TEST CASE NAME              : dtParamDummyValue13f.f
!*
!*  DATE                       : July 10, 2008
!*
!*  DESCRIPTION:
!*  The VALUE attribute specifies a type of argument association between a
!*  dummy argument and an actual argument. If the dummy argument has the
!*  VALUE attribute, it becomes associated with a definable anonymous data
!*  object whose initial value is that of the actual argument. Subsequent
!*  changes to the value or definition status of the dummy argument do not
!*  affect the actual argument.
!*
!*  CASE:
!*  When having DT with run-time known length DTP, and
!*  Have an allocatable array of the above DT with allocatable component,
!*  array element passed as actual argument
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  MODULE m1
  TYPE :: DT(K, L)
    INTEGER, KIND :: K=2
    INTEGER, LEN  :: L=3
    INTEGER(KIND=2), ALLOCATABLE :: i(:)
  END TYPE

  CONTAINS

  SUBROUTINE SUB1(T1)
  TYPE(DT), VALUE :: T1
  INTEGER :: m

  DO m=1,3
  IF((T1%i(m).ne.11).or.(T1%i%KIND.ne.2)) THEN
    print *, T1%i(m)
    STOP 1
  END IF
  END DO

  T1%i = [99,99,99]

  DO m=1,3
  IF((T1%i(m).ne.99).or.(T1%i%KIND.ne.2)) THEN
    print *, T1%i(m)
    STOP 2
  END IF
  END DO

  END SUBROUTINE
  END MODULE

  PROGRAM a
  USE m1

  TYPE(DT(2,:)), ALLOCATABLE :: TArr(:,:)
  INTEGER :: j,k,l

  ALLOCATE(DT::TArr(10,10))

  DO j=1, 10
  DO k=1, 10

    TArr(j,k)%i = [11,11,11]

    CALL SUB1(TArr(j,k))

    DO l=1, 3
      IF((TArr(j,k)%i(l).ne.11).or.(TArr(j,k)%i(l)%KIND.ne.2)) THEN
        print *, j, k, l
        print *, TArr(j,k)%i
        print *, TArr(j,k)%i%KIND
        STOP 3
      END IF
    END DO

  END DO
  END DO

  DEALLOCATE(TArr)

  END

