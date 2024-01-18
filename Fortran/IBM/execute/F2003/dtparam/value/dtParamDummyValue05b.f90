!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!* TEST CASE NAME              : dtParamDummyValue05b.f
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
!*  When the actual argument is a node of a linked list of DT, and A node
!*  created in the procedure points to the dummy argument
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

  TYPE(DT(2, 3)) :: T1
  TYPE(DT(2, 3)) :: T2
  TYPE(DT(2, 3)) :: T3
  TYPE(DT(2, 3)) :: T4
  TARGET :: T1, T2, T3

  T1%i = 11
  T2%i = 22
  T3%i = 33


  T1%ch = 'abc'
  T2%ch = 'def'
  T3%ch = 'ghi'

  T1%next => T2

  CALL Sub1(T1)

  IF ((T1%i.ne.11).or.(T1%ch.ne.'abc').or.(T2%i.ne.88).or.(T2%ch.ne.'uvw').or.(T3%i.ne.77).or.(T3%ch.ne.'rst').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3).or.(T2%i%KIND.ne.2).or.(T2%ch%LEN.ne.3).or.(T3%i%KIND.ne.2).or.(T3%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%ch, T2%i, T2%ch, T3%i, T3%ch
      print *, T1%i%KIND, T1%ch%LEN, T2%i%KIND, T2%ch%LEN, T3%i%KIND, T3%ch%LEN
      STOP 2
  END IF

  CONTAINS

  SUBROUTINE Sub1(T1)
    TYPE(DT(2, 3)), VALUE, TARGET  :: T1
    TYPE(DT(2, 3))  :: NT

    NT%next => T1

    NT%next%i = 99
    NT%next%ch = 'xyz'

    NT%next%next%i = 88
    NT%next%next%ch = 'uvw'

    NT%next%next => T3

    NT%next%next%i = 77
    NT%next%next%ch = 'rst'

    IF ((T1%i.ne.99).or.(T1%ch.ne.'xyz').or.(T2%i.ne.88).or.(T2%ch.ne.'uvw').or.(T3%i.ne.77).or.(T3%ch.ne.'rst').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3).or.(T2%i%KIND.ne.2).or.(T2%ch%LEN.ne.3).or.(T3%i%KIND.ne.2).or.(T3%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%ch, T2%i, T2%ch, T3%i, T3%ch
      print *, T1%i%KIND, T1%ch%LEN, T2%i%KIND, T2%ch%LEN, T3%i%KIND, T3%ch%LEN
      STOP 1
    END IF

  END SUBROUTINE

  END


