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
!* TEST CASE NAME              : dtParamDummyValue11h.f
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
!*  is allocatable, and an array element of DT which has an allocatable 
!*  component is passed as an actual argument
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  PROGRAM a

  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    INTEGER(KIND=K), ALLOCATABLE :: INTArr(:,:)
    CHARACTER(LEN=L), ALLOCATABLE :: CHArr(:,:)
  END TYPE

  TYPE(DT(2, 3)), ALLOCATABLE :: TArr(:,:)
  INTEGER :: j1, k1, j2, k2, m, n

  m = 5
  n = 10

  ALLOCATE(TArr(n,n))

  DO k1 = 1, n 
  DO j1 = 1, n 
    ALLOCATE(TArr(j1,k1)%INTArr(m,m))
    ALLOCATE(TArr(j1,k1)%CHArr(m,m))
    DO k2 = 1, m 
    DO j2 = 1, m
      TArr(j1,k1)%INTArr(j2,k2) = 11
      TArr(j1,k1)%CHArr(j2,k2) = 'abc'
    END DO
    END DO
  END DO
  END DO

  DO k1 = 1, n 
  Do j1 = 1, n 
    CALL Sub1(TArr(j1,k1), j1, k1, m)
  END DO
  END DO

  DO k1 = 1, n
  Do j1 = 1, n
    DO k2 = 1, m
    DO j2 = 1, m
      IF ((TArr(j1,k1)%INTArr(j2,k2).ne.11).or.(TArr(j1,k1)%CHArr(j2,k2).ne.'abc').or.(TArr(j1,k1)%INTArr(j2,k2)%KIND.ne.2).or.(TArr(j1,k1)%CHArr(j2,k2)%LEN.ne.3)) THEN
        print *, j1, k1, j2, k2
        print *, TArr(j1,k1)%INTArr(j2,k2), TArr(j1,k1)%CHArr(j2,k2)
        print *, TArr(j1,k1)%INTArr(j2,k2)%KIND, TArr(j1,k1)%CHArr(j2,k2)%LEN
        STOP 3
      END IF
    END DO
    END DO
  END DO
  END DO

  DO k1 = 1, n
  Do j1 = 1, n
    DEALLOCATE(TArr(j1,k1)%INTArr)
    DEALLOCATE(TArr(j1,k1)%CHArr)
  END DO
  END DO
  DEALLOCATE(TArr)

  CONTAINS

  SUBROUTINE Sub1(T1, j1, k1, m)
    TYPE(DT(2, 3)), VALUE  :: T1
    INTEGER :: j1, k1, j2, k2, m

    DO k2 = 1, m
    DO j2 = 1, m 

    IF ((T1%INTArr(j2,k2).ne.11).or.(T1%CHArr(j2,k2).ne.'abc').or.(T1%INTArr(j2,k2)%KIND.ne.2).or.(T1%CHArr(j2,k2)%LEN.ne.3)) THEN
      print *, j1, k1, j2, k2
      print *, T1%INTArr(j2,k2), T1%CHArr(j2,k2)
      print *, T1%INTArr(j2,k2)%KIND, T1%CHArr(j2,k2)%LEN
      STOP 1
    END IF

    T1%INTArr(j2,k2) = 99
    T1%CHArr(j2,k2) = 'xyz'

    IF ((T1%INTArr(j2,k2).ne.99).or.(T1%CHArr(j2,k2).ne.'xyz').or.(T1%INTArr(j2,k2)%KIND.ne.2).or.(T1%CHArr(j2,k2)%LEN.ne.3)) THEN
      print *, j1, k1, j2, k2
      print *, T1%INTArr(j2,k2), T1%CHArr(j2,k2)
      print *, T1%INTArr(j2,k2)%KIND, T1%CHArr(j2,k2)%LEN
      STOP 2
    END IF

    END DO
    END DO

  END SUBROUTINE

  END

