      TYPE Base  (l1)
        INTEGER, LEN  :: l1 = 1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2 = 2

        CLASS(Base(l2)), POINTER :: b_cmp
      END TYPE Child

      CLASS(Child(10,10)), POINTER :: b1

      ALLOCATE(b1)

      SELECT TYPE ( b1 )
        CLASS IS (Child(*,*))
            ALLOCATE(Base(10) :: b1%b_cmp)
            print*, b1%b_cmp%l1

      END SELECT
END
