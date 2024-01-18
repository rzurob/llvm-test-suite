     TYPE Base  (l1)
        INTEGER, LEN  :: l1

        CHARACTER(10)  :: my_type
        INTEGER :: my_arr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2

        CLASS(Base(l2)), POINTER :: c_cmp
      END TYPE Child

      CLASS(Base(5)), POINTER :: c2

      ALLOCATE(Child(5,5) :: c2)

      SELECT TYPE ( c2 )
        CLASS IS (Child(*,*))
            ALLOCATE(Base(c2%l2) :: c2%c_cmp)
            c2%c_cmp%my_type= 'Base'
            call allocate_auto(c2%c_cmp)

        CLASS DEFAULT
           STOP 29
      END SELECT

      CONTAINS

      SUBROUTINE allocate_auto(Arg)
      CLASS(Base(*)) ::  Arg
      CLASS(Base(:)), POINTER ::  Obj

         ALLOCATE(Obj, SOURCE = Arg)

         print*, Arg%my_type
         print*, Obj%my_type

      END SUBROUTINE allocate_auto
END

