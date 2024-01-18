!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!* TEST CASE NAME              : dtParamDummyValue07b.f
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
!*  When the actual argument is a DT with an allocatable component, and
!*  the dummy argument.s allocatable component is re-allocated. The size
!*  of the allocatable component will be checked
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
  INTEGER :: n1, n2

  n1 = 10
  n2 = 20

  ALLOCATE(T1%INTArr(n1,n1))
  ALLOCATE(T1%CHArr(n1,n1))

  IF ((sizeof(T1%INTArr).ne.200).or.(sizeof(T1%CHArr).ne.300)) THEN
    print *, sizeof(T1%INTArr), sizeof(T1%CHArr)
    STOP 1
  END IF

  CALL Sub1(T1, n2)

  IF ((sizeof(T1%INTArr).ne.200).or.(sizeof(T1%CHArr).ne.300)) THEN
    print *, sizeof(T1%INTArr), sizeof(T1%CHArr)
    STOP 3
  END IF

  DEALLOCATE(T1%INTArr)
  DEALLOCATE(T1%ChArr)

  CONTAINS

  SUBROUTINE Sub1(T1, n2)
    TYPE(DT(2, 3)), VALUE  :: T1
    INTEGER :: n2

    DEALLOCATE(T1%INTArr)
    DEALLOCATE(T1%ChArr)

    ALLOCATE(T1%INTArr(n2,n2))
    ALLOCATE(T1%CHArr(n2,n2))

    IF ((sizeof(T1%INTArr).ne.800).or.(sizeof(T1%CHArr).ne.1200)) THEN
      print *, sizeof(T1%INTArr), sizeof(T1%CHArr)
      STOP 2
    END IF

    DEALLOCATE(T1%INTArr)
    DEALLOCATE(T1%ChArr)

  END SUBROUTINE

  END

