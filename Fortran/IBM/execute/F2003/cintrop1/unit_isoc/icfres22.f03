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
      contains
        function f()
          use iso_c_binding
          type(c_ptr) :: f(5)
          f = c_loc(i)
        end function f
      end module m

      program icfres22
        use iso_c_binding
        use m
        type(c_ptr) :: cp(5)
        integer, pointer :: p1
        i = 42
        cp = f()
        call c_f_pointer(cp(3), p1)
        if (.not.c_associated(cp(3), c_loc(i))) error stop 1
        if (.not.c_associated(c_loc(i), cp(3))) error stop 6
        if (.not.c_associated(cp(3))) error stop 7
        if (.not.associated(p1)) error stop 10
        if (.not.associated(p1, i)) error stop 12
        if (p1 /= i) error stop 15
        if (p1 /= 42) error stop 17
        if (.not.c_associated(cp(3), c_loc(p1))) error stop 19
      end program icfres22