      ! Test case from defect 333766
      TYPE :: DT(L)
        INTEGER, LEN  :: L
        SEQUENCE
        CHARACTER(L):: C
      END TYPE

      TYPE(DT(:)), ALLOCATABLE :: T1

      ALLOCATE(DT(4) :: T1)
      END
