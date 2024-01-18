     TYPE Base  (l1)
        INTEGER, LEN  :: l1

        CHARACTER(l1)      :: tag(l1)
        INTEGER, ALLOCATABLE :: my_arr(:)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2

        CLASS(Base(l2)), ALLOCATABLE :: b_cmp
      END TYPE Child

      CLASS(Child(:,:)), POINTER :: b1

      ALLOCATE(Child(10,8) :: b1)

      SELECT TYPE ( b1 )
         CLASS IS (Child(*,*))
             ALLOCATE( Base(b1%l2) :: b1%b_cmp )

         CLASS DEFAULT
             STOP 36
      END SELECT
END
