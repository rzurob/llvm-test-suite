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
!*  element.s component, which is a DT with DTP, is passed as an
!*  actual argument
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  PROGRAM a

  TYPE :: DT2(K2, L2)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: L2
    INTEGER(KIND=K2) :: i2
    CHARACTER(LEN=L2) :: ch2
  END TYPE

  TYPE :: DT1(K1, L1)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: L1
    TYPE(DT2(4, 6)) :: subtype
  END TYPE

  TYPE(DT1(4, 6)), DIMENSION(10,10) :: TArr
  INTEGER :: j, k

  DO k = 1, 10
  DO j = 1, 10
    TArr(j,k)%subtype%i2 = 11
    TArr(j,k)%subtype%ch2 = 'abcdef'
  END DO
  END DO

  DO k = 1, 10
  DO j = 1, 10

    CALL Sub1(TArr(j,k)%subtype, j, k)

    IF ((TArr(j,k)%subtype%i2.ne.11).or.(TArr(j,k)%subtype%ch2.ne.'abcdef').or.(TArr(j,k)%subtype%i2%KIND.ne.4).or.(TArr(j,k)%subtype%ch2%LEN.ne.6)) THEN
      print *, j, k
      print *, TArr(j,k)%subtype%i2, TArr(j,k)%subtype%ch2
      print *, TArr(j,k)%subtype%i2%KIND, TArr(j,k)%subtype%ch2%LEN
      STOP 3
    END IF

  END DO
  END DO

  CONTAINS

  SUBROUTINE Sub1(T1, j, k)
    TYPE(DT2(4, 6)), VALUE :: T1
    INTEGER :: j, k

    IF ((T1%i2.ne.11).or.(T1%ch2.ne.'abcdef').or.(T1%i2%KIND.ne.4).or.(T1%ch2%LEN.ne.6)) THEN
      print *, j, k
      print *, T1%i2, T1%ch2
      print *, T1%i2%KIND, T1%ch2%LEN
      STOP 1
    END IF

    T1%i2 = 99
    T1%ch2 = 'uvwxyz'

    IF ((T1%i2.ne.99).or.(T1%ch2.ne.'uvwxyz').or.(T1%i2%KIND.ne.4).or.(T1%ch2%LEN.ne.6)) THEN
      print *, j, k
      print *, T1%i2, T1%ch2
      print *, T1%i2%KIND, T1%ch2%LEN
      STOP 2
    END IF

  END SUBROUTINE

  END
