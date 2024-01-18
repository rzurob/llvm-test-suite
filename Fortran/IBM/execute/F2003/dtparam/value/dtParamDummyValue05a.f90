!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!* TEST CASE NAME              : dtParamDummyValue05a.f
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
!*  When the actual argument is a node of a linked list of DT, and its
!*  dummy argument points to a node outside of the procedure
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
  TARGET :: T2, T3, T4

  T1%i = 11
  T2%i = 22
  T3%i = 33
  T4%i = 44

  T1%ch = 'abc'
  T2%ch = 'def'
  T3%ch = 'ghi'
  T4%ch = 'jkl'

  T1%next => T2
  T2%next => T4
  T3%next => T4

  CALL Sub1(T1)

    IF ((T1%i.ne.11).or.(T1%ch.ne.'abc').or.(T2%i.ne.88).or.(T2%ch.ne.'uvw').or.(T3%i.ne.77).or.(T3%ch.ne.'rst').or.(T1%next%i.ne.88).or.(T1%next%ch.ne.'uvw').or.(T1%next%next%i.ne.44).or.(T1%next%next%ch.ne.'jkl').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3).or.(T1%next%i%KIND.ne.2).or.(T1%next%ch%LEN.ne.3).or.(T1%next%next%i%KIND.ne.2).or.(T1%next%next%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%ch, T2%i, T2%ch, T3%i, T3%ch
      print *, T1%next%i, T1%next%ch, T1%next%next%i, T1%next%next%ch
      print *, T1%i%KIND, T1%ch%LEN, T1%next%i%KIND, T1%next%ch%LEN, T1%next%next%i%KIND, T1%next%next%ch%LEN
      STOP 3
    END IF


  CONTAINS

  SUBROUTINE Sub1(T1)
    TYPE(DT(2, 3)), VALUE  :: T1

    IF ((T1%i.ne.11).or.(T1%ch.ne.'abc').or.(T2%i.ne.22).or.(T2%ch.ne.'def').or.(T3%i.ne.33).or.(T3%ch.ne.'ghi').or.(T1%next%i.ne.22).or.(T1%next%ch.ne.'def').or.(T1%next%next%i.ne.44).or.(T1%next%next%ch.ne.'jkl').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3).or.(T1%next%i%KIND.ne.2).or.(T1%next%ch%LEN.ne.3).or.(T1%next%next%i%KIND.ne.2).or.(T1%next%next%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%ch, T2%i, T2%ch, T3%i, T3%ch
      print *, T1%next%i, T1%next%ch, T1%next%next%i, T1%next%next%ch
      print *, T1%i%KIND, T1%ch%LEN, T1%next%i%KIND, T1%next%ch%LEN, T1%next%next%i%KIND, T1%next%next%ch%LEN
      STOP 1
    END IF

    T1%i = 99
    T1%ch = 'xyz'
    T1%next%i = 88
    T1%next%ch = 'uvw'
    T1%next => T3
    T1%next%i = 77
    T1%next%ch = 'rst'

    IF ((T1%i.ne.99).or.(T1%ch.ne.'xyz').or.(T2%i.ne.88).or.(T2%ch.ne.'uvw').or.(T3%i.ne.77).or.(T3%ch.ne.'rst').or.(T1%next%i.ne.77).or.(T1%next%ch.ne.'rst').or.(T1%next%next%i.ne.44).or.(T1%next%next%ch.ne.'jkl').or.(T1%i%KIND.ne.2).or.(T1%ch%LEN.ne.3).or.(T1%next%i%KIND.ne.2).or.(T1%next%ch%LEN.ne.3).or.(T1%next%next%i%KIND.ne.2).or.(T1%next%next%ch%LEN.ne.3)) THEN
      print *, T1%i, T1%ch, T2%i, T2%ch, T3%i, T3%ch
      print *, T1%next%i, T1%next%ch, T1%next%next%i, T1%next%next%ch
      print *, T1%i%KIND, T1%ch%LEN, T1%next%i%KIND, T1%next%ch%LEN, T1%next%next%i%KIND, T1%next%next%ch%LEN
      STOP 2
    END IF

  END SUBROUTINE

  END


