     TYPE Base  (l1)
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: tag
        INTEGER, ALLOCATABLE :: my_arr(:)    !<--- If any if these components is removed, tc passes
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2

        CLASS(Base(l2)), ALLOCATABLE :: b_cmp
      END TYPE Child

      CLASS(Base(:)), POINTER :: b1

      ALLOCATE(Base(8) :: b1)

         SELECT TYPE ( b1 )
              CLASS IS (Child(*,*))
                 ALLOCATE(Base(b1%l2) :: b1%b_cmp)
         END SELECT
END

