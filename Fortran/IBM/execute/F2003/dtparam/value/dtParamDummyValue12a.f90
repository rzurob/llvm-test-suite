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
!* TEST CASE NAME              : dtParamDummyValue12a.f
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
!*  When having an object with extends attribute, and the parent type is 
!*  used as an actual argument 
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
    INTEGER (KIND=K2) :: i
    CHARACTER (LEN=L2) :: ch2
  END TYPE

  TYPE(PARENT(2, 3)) :: T1
  T1%i1 = 11
  T1%ch1 = 'abc'

  CALL Sub1(T1)

  IF ((T1%i1.ne.11).or.(T1%ch1.ne.'abc').or.(T1%i1%KIND.ne.2).or.(T1%ch1%LEN.ne.3)) THEN
    print *, T1%i1, T1%ch1
    print *, T1%i1%KIND, T1%ch1%LEN
    STOP 3
  END IF

  CONTAINS
  SUBROUTINE Sub1(T1)
    TYPE(PARENT(2, 3)), VALUE :: T1

    IF ((T1%i1.ne.11).or.(T1%ch1.ne.'abc').or.(T1%i1%KIND.ne.2).or.(T1%ch1%LEN.ne.3)) THEN
      print *, T1%i1, T1%ch1
      print *, T1%i1%KIND, T1%ch1%LEN
      STOP 1
    END IF

    T1%i1 = 99
    T1%ch1 = 'xyz'

    IF ((T1%i1.ne.99).or.(T1%ch1.ne.'xyz').or.(T1%i1%KIND.ne.2).or.(T1%ch1%LEN.ne.3)) THEN
      print *, T1%i1, T1%ch1
      print *, T1%i1%KIND, T1%ch1%LEN
      STOP 2
    END IF

  END SUBROUTINE

  END

