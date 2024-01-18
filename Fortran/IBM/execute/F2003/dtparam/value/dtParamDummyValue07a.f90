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
!* TEST CASE NAME              : dtParamDummyValue07a.f
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
!*  When the actual argument is a DT with an allocatable component, and
!*  The allocatable component is allocated in the main program
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  PROGRAM a

  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    INTEGER (KIND=K), ALLOCATABLE :: INTArr(:,:)
    CHARACTER (LEN=L), ALLOCATABLE :: CHArr(:,:)
  END TYPE

  TYPE(DT(2, 3)) :: T1
  INTEGER :: j, k, n

  n = 10

  ALLOCATE(T1%INTArr(n,n))
  ALLOCATE(T1%CHArr(n,n))

  DO k = 1, 10
  DO j = 1, 10
    T1%INTArr(j,k) = 11
    T1%CHArr(j,k) = 'abc'
  END DO
  END DO

  CALL Sub1(T1)

  DO k = 1, 10
  DO j = 1, 10
  IF ((T1%INTArr(j,k).ne.11).or.(T1%CHArr(j,k).ne.'abc').or.(T1%INTArr(j,k)%KIND.ne.2).or.(T1%CHArr(j,k)%LEN.ne.3)) THEN
    print *, T1%INTArr(j,k), T1%CHArr(j,k)
    print *, T1%INTArr(j,k)%KIND, T1%CHArr(j,k)%LEN
    STOP 3
  END IF
  END DO
  END DO

  DEALLOCATE(T1%INTArr)
  DEALLOCATE(T1%ChArr)

  CONTAINS

  SUBROUTINE Sub1(T1)
    TYPE(DT(2, 3)), VALUE  :: T1
    INTEGER :: j, k

    DO k = 1, 10
    DO j = 1, 10
    IF ((T1%INTArr(j,k).ne.11).or.(T1%CHArr(j,k).ne.'abc').or.(T1%INTArr(j,k)%KIND.ne.2).or.(T1%CHArr(j,k)%LEN.ne.3)) THEN
      print *, T1%INTArr(j,k), T1%CHArr(j,k)
      print *, T1%INTArr(j,k)%KIND, T1%CHArr(j,k)%LEN
      STOP 1
    END IF
    END DO
    END DO

    DO k = 1, 10
    DO j = 1, 10
      T1%INTArr(j,k) = 99
      T1%CHArr(j,k) = 'xyz'
    END DO
    END DO

    DO k = 1, 10
    DO j = 1, 10
    IF ((T1%INTArr(j,k).ne.99).or.(T1%CHArr(j,k).ne.'xyz').or.(T1%INTArr(j,k)%KIND.ne.2).or.(T1%CHArr(j,k)%LEN.ne.3)) THEN
      print *, T1%INTArr(j,k), T1%CHArr(j,k)
      print *, T1%INTArr(j,k)%KIND, T1%CHArr(j,k)%LEN
      STOP 2
    END IF
    END DO
    END DO

  END SUBROUTINE

  END

