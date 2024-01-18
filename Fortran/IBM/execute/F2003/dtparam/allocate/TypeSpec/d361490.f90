     TYPE Base  (l1)
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: tag   
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2

        CLASS(Base(l2)), ALLOCATABLE :: b_cmp
      END TYPE Child

      CLASS(Base(:)), ALLOCATABLE :: b1

      ALLOCATE(Child(10,20) :: b1)

      SELECT TYPE ( b1 )
           CLASS IS (Child(*,*))
              ALLOCATE(Base(b1%l2) :: b1%b_cmp)
              DEALLOCATE(b1%b_cmp)

           CLASS DEFAULT
              STOP 10
      END SELECT
END
