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
!* TEST CASE NAME              : dtParamDummyValue11e.f
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
!*  When having an array of DT, and the array is allocatable, and the array 
!*  is a pointer array, and an array element is passed as an actual argument
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  PROGRAM a

  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    INTEGER(KIND=K), POINTER :: INTPtr
    CHARACTER(LEN=L), POINTER :: CHPtr
  END TYPE

  TYPE(DT(2, 3)), DIMENSION(10) :: TArr
  INTEGER :: j

  INTEGER(2), TARGET :: INTTar1, INTTar2
  CHARACTER(3), TARGET :: CHTar1, CHTar2

  INTTar1 = 11
  INTTar2 = 99
  CHTar1 = 'abc'
  CHTar2 = 'xyz'

  DO j = 1, 10
    TArr(j)%INTPtr => INTTar1
    TArr(j)%CHPtr => CHTar1
  END DO

  Do j = 1, 10
    CALL Sub1(TArr(j))
    IF ((TArr(j)%INTPtr.ne.11).or.(TArr(j)%CHPtr.ne.'abc').or.(TArr(j)%INTPtr%KIND.ne.2).or.(TArr(j)%CHPtr%LEN.ne.3))  THEN
      print *, TArr(j)%INTPtr, TArr(j)%CHPtr
      print *, TArr(j)%INTPtr%KIND, TArr(j)%CHPtr%LEN
      STOP 3
    END IF
  END DO

  CONTAINS

  SUBROUTINE Sub1(T1)
    TYPE(DT(2, 3)), VALUE  :: T1

    IF ((T1%INTPtr.ne.11).or.(T1%CHPtr.ne.'abc').or.(T1%INTPtr%KIND.ne.2).or.(T1%CHPtr%LEN.ne.3))  THEN
      print *, T1%INTPtr, T1%CHPtr
      print *, T1%INTPtr%KIND, T1%CHPtr%LEN
      STOP 1
    END IF

    T1%INTPtr => INTTar2
    T1%CHPtr => CHTar2

    IF ((T1%INTPtr.ne.99).or.(T1%CHPtr.ne.'xyz').or.(T1%INTPtr%KIND.ne.2).or.(T1%CHPtr%LEN.ne.3))  THEN
      print *, T1%INTPtr, T1%CHPtr
      print *, T1%INTPtr%KIND, T1%CHPtr%LEN
      STOP 1
    END IF

  END SUBROUTINE

  END

