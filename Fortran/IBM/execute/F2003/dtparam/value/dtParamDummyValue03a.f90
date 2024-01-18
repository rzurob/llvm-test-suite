!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!* TEST CASE NAME              : dtParamDummyValue03a.f
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
!*  When having an external procedure, and its actual argument is a DT
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  MODULE m
  TYPE :: DT(K,L)
    INTEGER, KIND :: K=2
    INTEGER, LEN  :: L=3
    INTEGER (KIND=K) :: i
    CHARACTER (LEN=L) :: ch
  END TYPE
  END MODULE

  PROGRAM a
    USE m

  INTERFACE
    SUBROUTINE ExtSub(T1)
      IMPORT :: DT
      TYPE(DT), VALUE :: T1
    END SUBROUTINE
  END INTERFACE

  TYPE(DT) :: T1
  T1%i = 11
  T1%ch = 'abc'

  CALL ExtSub(T1)

  IF ((T1%i.ne.11) .or. (T1%i%KIND.ne.2) .or. (T1%ch.ne.'abc') .or. (T1%ch%LEN.ne.3)) THEN
    print *, T1%i, T1%i%KIND, T1%ch, T1%ch%LEN
    STOP 3
  END IF

  END

  SUBROUTINE ExtSub(T1)
    USE m

    TYPE(DT), VALUE :: T1

    IF ((T1%i.ne.11) .or. (T1%i%KIND.ne.2) .or. (T1%ch.ne.'abc') .or. (T1%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%i%KIND, T1%ch, T1%ch%LEN
      STOP 1
    END IF

    T1%i = 99
    T1%ch = 'xyz'

    IF ((T1%i.ne.99) .or. (T1%i%KIND.ne.2) .or. (T1%ch.ne.'xyz') .or. (T1%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%i%KIND, T1%ch, T1%ch%LEN
      STOP 2
    END IF

  END SUBROUTINE

