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
!*  When the procedure is a function that takes a DT as the actual
!*  argument and returns a DT
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  PROGRAM a

  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    INTEGER(KIND=K) :: i
    CHARACTER(LEN=L) :: ch
  END TYPE

  TYPE(DT(2, 3)) :: T1
  TYPE(DT(2, 3)) :: T2

  T1%i = 11
  T1%ch = 'abc'

  T2 = Func1(T1)

    IF ((T1%i.ne.11).or.(T1%ch.ne.'abc').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3).or.(T2%i.ne.99).or.(T2%ch.ne.'xyz').or.(T2%i%KIND.ne.2).or.(T2%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%ch, T2%i, T2%ch
      print *, T1%i%KIND, T1%ch%LEN, T2%i%KIND, T2%ch%LEN
      STOP 3
    END IF

  CONTAINS

  TYPE(DT(2, 3)) FUNCTION Func1(T1)
    TYPE(DT(2, 3)), VALUE  :: T1

    IF ((T1%i.ne.11).or.(T1%ch.ne.'abc').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%ch
      print *, T1%i%KIND, T1%ch%LEN
      STOP 1
    END IF

    T1%i = 99
    T1%ch = 'xyz'
    Func1 = T1

    IF ((T1%i.ne.99).or.(T1%ch.ne.'xyz').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3).or.(Func1%i.ne.99).or.(Func1%ch.ne.'xyz').or.(Func1%i%KIND.ne.2).or.(Func1%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%ch, Func1%i, Func1%ch
      print *, T1%i%KIND, T1%ch%LEN, Func1%i%KIND, Func1%ch%LEN
      STOP 2
    END IF

  END FUNCTION

  END
