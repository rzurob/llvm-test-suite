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
      contains
        function f()
          use iso_c_binding
          type(c_funptr) :: f
          f = c_null_funptr
        end function f
      end module

      program icfres23
        use iso_c_binding
        use m
        type(c_funptr) :: cp
        cp = f()
        if (c_associated(cp)) error stop 1
        if (c_associated(f())) error stop 2
        if (c_associated(f(), f())) error stop 3
        if (c_associated(f(), cp)) error stop 4
        if (c_associated(cp, f())) error stop 5
      end program icfres23