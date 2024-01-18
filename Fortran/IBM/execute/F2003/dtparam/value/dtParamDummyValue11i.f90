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
!*  When having an array of DT, and the array is allocatable, and an array
!*  element of a DT linked list is passed as an actual argument
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  PROGRAM a

  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    INTEGER(KIND=K) :: i
    CHARACTER(LEN=L) :: ch
    TYPE(DT(K,L)), POINTER :: next
  END TYPE

  TYPE(DT(2, 3)), DIMENSION(10,10) :: TArr
  INTEGER :: j, k

  TYPE(DT(2, 3)) :: Thead1, Thead2
  TARGET :: Thead1, Thead2
  Thead1%i = 0
  Thead1%ch = 'hd1'
  Thead2%i = 100
  Thead2%ch = 'hd2'

  DO k = 1, 10
  DO j = 1, 10
    TArr(j,k)%i = 11
    TArr(j,k)%ch = 'abc'
    TArr(j,k)%next => Thead1
  END DO
  END DO

  DO k = 1, 10
  DO j = 1, 10

    CALL Sub1(TArr(j,k), j, k)

  IF ((TArr(j,k)%i.ne.11).or.(TArr(j,k)%ch.ne.'abc').or.(TArr(j,k)%next%i.ne.0).or.(TArr(j,k)%next%ch.ne.'hd1').or.(TArr(j,k)%i%KIND.ne.2).or.(TArr(j,k)%ch%LEN.ne.3).or.(TArr(j,k)%next%i%KIND.ne.2).or.(TArr(j,k)%next%ch%LEN.ne.3)) THEN
    print *, j, k
    print *, TArr(j,k)%i, TArr(j,k)%ch, TArr(j,k)%next%i, TArr(j,k)%next%ch
    print *, Thead1%i, Thead1%ch
    print *, TArr(j,k)%i%KIND, TArr(j,k)%ch%LEN, TArr(j,k)%next%i%KIND, TArr(j,k)%next%ch%LEN, Thead1%i%KIND, Thead1%ch%LEN
    STOP 3
  END IF

  END DO
  END DO

  CONTAINS

  SUBROUTINE Sub1(T1, j, k)
    TYPE(DT(2, 3)), VALUE  :: T1
    INTEGER :: j, k

    IF ((T1%i.ne.11).or.(T1%ch.ne.'abc').or.(T1%next%i.ne.0).or.(T1%next%ch.ne.'hd1').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3).or.(T1%next%i%KIND.ne.2).or.(T1%next%ch%LEN.ne.3)) THEN
      print *, j, k
      print *, T1%i, T1%ch, T1%next%i, T1%next%ch
      print *, Thead1%i, Thead1%ch
      print *, T1%i%KIND, T1%ch%LEN, T1%next%i%KIND, T1%next%ch%LEN, Thead1%i%KIND, Thead1%ch%LEN
      STOP 1
    END IF

    T1%i = 99
    T1%ch = 'xyz'
    T1%next => Thead2

    IF ((T1%i.ne.99).or.(T1%ch.ne.'xyz').or.(T1%next%i.ne.100).or.(T1%next%ch.ne.'hd2').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3).or.(T1%next%i%KIND.ne.2).or.(T1%next%ch%LEN.ne.3)) THEN
      print *, j, k
      print *, T1%i, T1%ch, T1%next%i, T1%next%ch
      print *, Thead1%i, Thead1%ch
      print *, T1%i%KIND, T1%ch%LEN, T1%next%i%KIND, T1%next%ch%LEN, Thead1%i%KIND, Thead1%ch%LEN
      STOP 2
    END IF

  END SUBROUTINE

  END

