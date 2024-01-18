!**********************************************************************
!*  ===================================================================
!*
!*                               ISO_C_BINDING module.
!*
!*  DATE                       : June 12, 2003
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  06/12/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
      module m
        integer, target :: i
      end module m

      program icfres01
        use iso_c_binding
        use m
        interface
          function f()
            use iso_c_binding
            type(c_ptr) :: f
          end function
        end interface
        type(c_ptr) :: cp
        integer, pointer :: p1, p2
        i = 42
        cp = f()
        call c_f_pointer(cp, p1)
        call c_f_pointer(f(), p2)
        if (.not.c_associated(cp, c_loc(i))) error stop 1
        if (.not.c_associated(cp, f())) error stop 2
        if (.not.c_associated(f(), f())) error stop 3
        if (.not.c_associated(f(), c_loc(i))) error stop 4
        if (.not.c_associated(c_loc(i), f())) error stop 5
        if (.not.c_associated(c_loc(i), cp)) error stop 6
        if (.not.c_associated(cp)) error stop 7
        if (.not.c_associated(f())) error stop 8
        if (.not.associated(p1, p2)) error stop 9
        if (.not.associated(p1)) error stop 10
        if (.not.associated(p2)) error stop 11
        if (.not.associated(p1, i)) error stop 12
        if (.not.associated(p2, i)) error stop 13
        if (p1 /= p2) error stop 14
        if (p1 /= i) error stop 15
        if (p2 /= i) error stop 16
        if (p1 /= 42) error stop 17
        if (p2 /= 42) error stop 18
        if (.not.c_associated(cp, c_loc(p1))) error stop 19
        if (.not.c_associated(cp, c_loc(p2))) error stop 20
      end program icfres01

      function f()
        use iso_c_binding
        use m
        type(c_ptr) :: f
        f = c_loc(i)
      end function f
