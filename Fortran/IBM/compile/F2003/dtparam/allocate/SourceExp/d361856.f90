      TYPE Base  (l1)
        INTEGER, LEN  :: l1

        INTEGER :: my_arr(l1)
        CHARACTER(2*l1)  :: my_type   
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2

        CLASS(Base(l2)), POINTER :: cmp
      END TYPE Child

      CLASS(Base(5)), POINTER :: c2

      ALLOCATE(Child(5,5) :: c2)

      SELECT TYPE ( c2 )
        CLASS IS (Child(*,*))
            ALLOCATE(Base(c2%l2) :: c2%cmp)
            c2%cmp%my_type= 'Base'   
      END SELECT

END
