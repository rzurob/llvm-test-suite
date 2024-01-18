      TYPE Base  (l1)
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: tag
        INTEGER, ALLOCATABLE :: my_arr(:)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2

        TYPE(Base(l2)), ALLOCATABLE :: b_cmp
      END TYPE Child

      INTEGER :: i
      CLASS(Child(10,8)), POINTER :: b1

      ALLOCATE(b1)

      SELECT TYPE ( b1 )
           CLASS IS (Child(*,*))
              ALLOCATE(Base(b1%l2) :: b1%b_cmp)
              ALLOCATE(b1%b_cmp%my_arr(b1%b_cmp%l1), SOURCE=(/(i, i = 1, b1%b_cmp%l1)/))

           CLASS DEFAULT
              STOP 45
      END SELECT
END

