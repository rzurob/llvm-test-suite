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
!*  When having an object with extends attribute, and the child type
!*  is used as an actual argument
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  PROGRAM a

  TYPE :: PARENT(K1, L1)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: L1
    INTEGER (KIND=K1) :: i1
    CHARACTER (LEN=L1) :: ch1
  END TYPE

  TYPE, EXTENDS(PARENT) :: CHILD(K2, L2)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: L2
    INTEGER (KIND=K2) :: i2
    CHARACTER (LEN=L2) :: ch2
  END TYPE

  TYPE(CHILD(2, 3, 4, 6)) :: T1

  T1%i1 = 11
  T1%ch1 = 'abc'
  T1%i2 = 22
  T1%ch2 = 'defghi'

  CALL Sub1(T1)

  IF ((T1%i1.ne.11).or.(T1%ch1.ne.'abc').or.(T1%i2.ne.22).or.(T1%ch2.ne.'defghi').or.(T1%i1%KIND.ne.2).or.(T1%ch1%LEN.ne.3).or.(T1%i2%KIND.ne.4).or.(T1%ch2%LEN.ne.6)) THEN
    print *, T1%i1, T1%ch1, T1%i2, T1%ch2
    print *, T1%i1%KIND, T1%ch1%LEN, T1%i2%KIND, T1%ch2%LEN
    STOP 3
  END IF

  CONTAINS
  SUBROUTINE Sub1(T1)
    TYPE(CHILD(2, 3, 4, 6)), VALUE :: T1

    IF ((T1%i1.ne.11).or.(T1%ch1.ne.'abc').or.(T1%i2.ne.22).or.(T1%ch2.ne.'defghi').or.(T1%i1%KIND.ne.2).or.(T1%ch1%LEN.ne.3).or.(T1%i2%KIND.ne.4).or.(T1%ch2%LEN.ne.6)) THEN
      print *, T1%i1, T1%ch1, T1%i2, T1%ch2
      print *, T1%i1%KIND, T1%ch1%LEN, T1%i2%KIND, T1%ch2%LEN
      STOP 1
    END IF

    T1%i1 = 99
    T1%ch1 = 'xyz'
    T1%i2 = 88
    T1%ch2 = 'rstuvw'

    IF ((T1%i1.ne.99).or.(T1%ch1.ne.'xyz').or.(T1%i2.ne.88).or.(T1%ch2.ne.'rstuvw ').or.(T1%i1%KIND.ne.2).or.(T1%ch1%LEN.ne.3).or.(T1%i2%KIND.ne.4).or.(T1%ch2%LEN.ne.6)) THEN
      print *, T1%i1, T1%ch1, T1%i2, T1%ch2
      print *, T1%i1%KIND, T1%ch1%LEN, T1%i2%KIND, T1%ch2%LEN
      STOP 2
    END IF

  END SUBROUTINE

  END

