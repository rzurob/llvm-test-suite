      TYPE Base  (l1)
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: Carr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2

        CLASS(Base(l2)), POINTER :: ptr
      END TYPE Child

      TYPE Branch  (n1,n2,n3)
        INTEGER, LEN  :: n1, n2, n3

        TYPE(Child(n1,n2)) :: cmp1(n3)
      END TYPE Branch

      CLASS(Branch(:,:,:)), POINTER :: b0

      ALLOCATE( Branch(2,5,2) :: b0 )
      IF ( .NOT. ASSOCIATED(b0) ) ERROR STOP 10

      b0%cmp1 = Child(2,5) ('B', NULL())

      ALLOCATE( Base(5) :: b0%cmp1(2)%ptr )
      IF ( .NOT. ASSOCIATED(b0%cmp1(2)%ptr) ) ERROR STOP 11
END
