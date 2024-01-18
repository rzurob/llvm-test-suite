      TYPE Base  (l1)
        INTEGER, LEN  :: l1 = 10
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2 = 10
        CLASS(Base(l1+l2)), POINTER :: b_cmp
      END TYPE Child

      TYPE(Child(:,:)), ALLOCATABLE :: c1

      ALLOCATE(Child(100,100):: c1)

      ALLOCATE(Base(200):: c1%b_cmp)
      print*, c1%b_cmp%l1

END
