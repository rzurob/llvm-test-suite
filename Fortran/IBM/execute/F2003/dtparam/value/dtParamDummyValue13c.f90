!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!* TEST CASE NAME              : dtParamDummyValue13c.f
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
!*  DT component's element is passed as actual argument
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  MODULE m
  TYPE :: DT(K,L)
    INTEGER, KIND :: K=2
    INTEGER, LEN  :: L=3
    INTEGER(KIND=2)  :: i(L)
  END TYPE
  END MODULE

  PROGRAM a
  USE m

  INTERFACE
  SUBROUTINE SUB1(i, j)
    INTEGER(KIND=2), VALUE :: i
    INTEGER :: j
  END SUBROUTINE
  END INTERFACE

  TYPE(DT(2,:)), ALLOCATABLE :: T1

  ALLOCATE(DT::T1)

  T1%i = [11,11,11]

  DO j=1, 3
    CALL SUB1(T1%i(j), j)
    IF((T1%i(j).ne.11).or.(T1%i(j)%KIND.ne.2)) THEN
      print *, j
      print *, T1%i(j)
      print *, T1%i(j)%KIND
      STOP 3
    END IF
  END DO

  DEALLOCATE(T1)

  END

  SUBROUTINE SUB1(i, j)
  USE m

  INTEGER(KIND=2), VALUE :: i
  INTEGER :: j

  IF((i.ne.11).or.(i%KIND.ne.2)) THEN
    print *, j
    print *, i
    print *, i%KIND
    STOP 1
  END IF

  i = 99

  IF((i.ne.99).or.(i%KIND.ne.2)) THEN
    print *, j
    print *, i
    print *, i%KIND
    STOP 2
  END IF

  END SUBROUTINE

