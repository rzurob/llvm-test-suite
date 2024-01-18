! *********************************************************************
!*  ===================================================================
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
!*  When the actual argument is a DT with a procedure pointer component
!*  that has a VALUE attribute in its dummy argument
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  MODULE m
  TYPE :: DT(K,L)
    INTEGER, KIND :: K=2
    INTEGER, LEN  :: L=3
    INTEGER (KIND=K) :: i
    CHARACTER (LEN=L) :: ch
    PROCEDURE(PROCPTR1), POINTER, NOPASS :: p1 => null()
  END TYPE

  CONTAINS
  SUBROUTINE PROCPTR1(T1)
    TYPE(DT(2, 3)), VALUE  :: T1

    T1%i = 88
    T1%ch = 'uvw'

    IF ((T1%i.ne.88).or.(T1%ch.ne.'uvw').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3)) THEN
      print *, T1%i,T1%ch
      print *, T1%i%KIND, T1%ch%LEN
      STOP 4
    END IF

  END SUBROUTINE

  END MODULE

  PROGRAM a
  USE m

  TYPE(DT(2, 3)) :: T1

  T1%i = 11
  T1%ch = 'abc'
  T1%p1 => PROCPTR1

  CALL PROCPTR1(T1)

  IF ((T1%i.ne.11).or.(T1%ch.ne.'abc').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3)) THEN
    print *, T1%i,T1%ch
    print *, T1%i%KIND, T1%ch%LEN
    STOP 1
  END IF

  CALL Sub1(T1)

  IF ((T1%i.ne.11).or.(T1%ch.ne.'abc').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3)) THEN
    print *, T1%i,T1%ch
    print *, T1%i%KIND, T1%ch%LEN
    STOP 6
  END IF

  CONTAINS
  SUBROUTINE Sub1(T1)
    TYPE(DT(2, 3)), VALUE :: T1

    IF ((T1%i.ne.11).or.(T1%ch.ne.'abc').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3)) THEN
      print *, T1%i,T1%ch
      print *, T1%i%KIND, T1%ch%LEN
      STOP 2
    END IF

    T1%i = 99
    T1%ch = 'xyz'

    IF ((T1%i.ne.99).or.(T1%ch.ne.'xyz').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3)) THEN
      print *, T1%i,T1%ch
      print *, T1%i%KIND, T1%ch%LEN
      STOP 3
    END IF

    CALL PROCPTR1(T1)

    IF ((T1%i.ne.99).or.(T1%ch.ne.'xyz').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3)) THEN
      print *, T1%i,T1%ch
      print *, T1%i%KIND, T1%ch%LEN
      STOP 5
    END IF

  END SUBROUTINE

  END

