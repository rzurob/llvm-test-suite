!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!* TEST CASE TITLE : Functional test for DTP dummy argument with VALUE
!*
!* TEST CASE NAME              : dtParamDummyValue13a.f
!*
!*  PROGRAMMER                 : Andy Sheung
!*  DATE                       : July 10, 2008
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
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
!*  When having DT with run-time known length DTP, and
!*  DT component's LEN DTP is defined in DT definition, DT passed as 
!*  actual argument
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  PROGRAM a

  TYPE :: DT(K, L)
    INTEGER, KIND :: K=2
    INTEGER, LEN  :: L=3
    INTEGER(KIND=K)  :: i(L)
  END TYPE

  TYPE(DT(2,:)), ALLOCATABLE :: T1

  ALLOCATE(DT::T1)

  T1%i = [1,2,3]

  CALL SUB1(T1)

  IF((T1%i(1).ne.1).or.(T1%i(2).ne.2).or.(T1%i(3).ne.3).or.(T1%i%KIND.ne.2)) THEN
    print *, T1%i
    print *, T1%i%KIND
    STOP 3
  END IF

  DEALLOCATE(T1)

  CONTAINS

  SUBROUTINE SUB1(T1)
  TYPE(DT), VALUE :: T1

  IF((T1%i(1).ne.1).or.(T1%i(2).ne.2).or.(T1%i(3).ne.3).or.(T1%i%KIND.ne.2)) THEN
    print *, T1%i
    STOP 1
  END IF

  T1%i = [4,5,6]

  IF((T1%i(1).ne.4).or.(T1%i(2).ne.5).or.(T1%i(3).ne.6).or.(T1%i%KIND.ne.2)) THEN
    print *, T1%i
    STOP 2
  END IF

  END SUBROUTINE

  END

