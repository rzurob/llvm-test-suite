      TYPE Base  (l1)
        INTEGER, LEN  :: l1

        INTEGER, ALLOCATABLE :: my_arr(:)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        CLASS(Base(l1+l2)), POINTER :: b_cmp
      END TYPE Child

      TYPE Branch  (q3,n1,n2)
        INTEGER, KIND :: q3
        INTEGER, LEN  :: n1, n2

        TYPE(Child(n2,q3,n2)) :: c_cmp(n1)
      END TYPE Branch

      CLASS(Branch(4,:,:)), ALLOCATABLE :: b1(:)

      ALLOCATE(Branch(4,2,4):: b1(1))

      ALLOCATE(Child(8,4,12) :: b1(1)%c_cmp(1)%b_cmp)

      IF (b1(1)%c_cmp(1)%b_cmp%l1 /= 8) ERROR STOP 100
END
