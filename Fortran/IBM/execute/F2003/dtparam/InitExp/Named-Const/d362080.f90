      TYPE Base  (l1)
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: Carr(l1)
        REAL          :: Rarr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2

        TYPE(Base(l2)), POINTER :: ptr
      END TYPE Child

      TYPE Branch  (n1,n2)
        INTEGER, LEN  :: n1, n2

        TYPE(Child(n1,n2)) :: cmp1(n1)
      END TYPE Branch

      CLASS(Branch(:,:)), POINTER :: b0

      ALLOCATE( Branch(2,5) :: b0 )

      b0%cmp1 = Child(2,5) ('BB', 4.8, NULL())

      ALLOCATE( Base(5) :: b0%cmp1(1)%ptr )
      b0%cmp1(1)%ptr%Rarr = 1.0
END
