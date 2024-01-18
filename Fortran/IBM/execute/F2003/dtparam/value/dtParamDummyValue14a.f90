!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!* TEST CASE NAME              : dtParamDummyValue14a.f
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
!*  Having a pointer component with DTP in a DT defined in a module, with
!*  the pointer component allocated in the main, pass the DT as a dummy
!*  argument of an external subroutine, and modify the pointer in the
!*  subroutine
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  MODULE m
  TYPE :: DT(K,L)
    INTEGER, KIND :: K=2
    INTEGER, LEN  :: L=3
    INTEGER (KIND=K), POINTER :: i
    CHARACTER (LEN=L), POINTER :: ch
  END TYPE
  END MODULE

  PROGRAM a
  USE m

  INTERFACE
  SUBROUTINE Sub1(T1, itar2, chtar2)
    IMPORT :: DT
    TYPE(DT), VALUE :: T1
    INTEGER(2), TARGET :: itar2
    CHARACTER(3), TARGET :: chtar2
  END SUBROUTINE
  END INTERFACE

  TYPE(DT) :: T1
  INTEGER(2), TARGET :: itar = 11
  CHARACTER(3), TARGET :: chtar = 'abc'
  INTEGER(2), TARGET :: itar2 = 99
  CHARACTER(3), TARGET :: chtar2 = 'xyz'

  allocate(T1%i)
  allocate(T1%ch)

  T1%i => itar
  T1%ch => chtar

  CALL Sub1(T1, itar2, chtar2)

  IF ((T1%i.ne.11) .or. (T1%i%KIND.ne.2) .or. (T1%ch.ne.'abc') .or. (T1%ch%LEN.ne.3)) THEN
    print *, T1%i, T1%i%KIND, T1%ch, T1%ch%LEN
    STOP 3
  END IF

  END

  SUBROUTINE Sub1(T1, itar2, chtar2)
    USE m
    TYPE(DT), VALUE :: T1
    INTEGER(2), TARGET :: itar2
    CHARACTER(3), TARGET :: chtar2

    IF ((T1%i.ne.11) .or. (T1%i%KIND.ne.2) .or. (T1%ch.ne.'abc') .or. (T1%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%i%KIND, T1%ch, T1%ch%LEN
      STOP 1
    END IF

    T1%i => itar2
    T1%ch => chtar2

    IF ((T1%i.ne.99) .or. (T1%i%KIND.ne.2) .or. (T1%ch.ne.'xyz') .or. (T1%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%i%KIND, T1%ch, T1%ch%LEN
      STOP 2
    END IF

  END SUBROUTINE


