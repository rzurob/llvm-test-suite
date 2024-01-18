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
!*  element.s component is passed as an actual argument
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

  TYPE(DT(2, 3)), ALLOCATABLE :: TArr(:,:)
  INTEGER :: j, k, n

  n = 10

  ALLOCATE(TArr(n,n))

  DO k = 1, n
  DO j = 1, n
    TArr(j,k)%i = 11
    TArr(j,k)%ch = 'abc'
  END DO
  END DO

  DO k = 1, n
  Do j = 1, n

    CALL Sub1(TArr(j,k)%i, TArr(j,k)%ch)

    IF ((TArr(j,k)%i.ne.11).or.(TArr(j,k)%ch.ne.'abc').or.(TArr(j,k)%i%KIND.ne.2).or.(TArr(j,k)%ch%LEN.ne.3)) THEN
      print *, j, k
      print *, TArr(j,k)%i, TArr(j,k)%ch
      print *, TArr(j,k)%i%KIND, TArr(j,k)%ch%LEN
      STOP 3
    END IF

  END DO
  END DO

  DEALLOCATE(TArr)

  CONTAINS

  SUBROUTINE Sub1(i, ch)
    INTEGER(KIND=2), VALUE :: i
    CHARACTER(LEN=3), VALUE :: ch

    IF ((i.ne.11).or.(ch.ne.'abc').or.(i%KIND.ne.2).or.(ch%LEN.ne.3)) THEN
      print *, j, k
      print *, i, ch
      print *, i%KIND, ch%LEN
      STOP 1
    END IF

    i = 99
    ch = 'xyz'

    IF ((i.ne.99).or.(ch.ne.'xyz').or.(i%KIND.ne.2).or.(ch%LEN.ne.3)) THEN
      print *, j, k
      print *, i, ch
      print *, i%KIND, ch%LEN
      STOP 2
    END IF

  END SUBROUTINE

  END


