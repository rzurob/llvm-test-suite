      TYPE Base  (l1)
        INTEGER, LEN  :: l1

        INTEGER, ALLOCATABLE :: my_arr(:)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2

        CLASS(Base(l1+l2)), POINTER :: b_cmp
      END TYPE Child

      TYPE(Child(:,:)), ALLOCATABLE :: c1

      ALLOCATE(Child(100,100):: c1)

      ALLOCATE(Base(200) :: c1%b_cmp)

      ALLOCATE(c1%b_cmp%my_arr(c1%b_cmp%l1), SOURCE=(/(i, i = 1, c1%b_cmp%l1)/))
END
