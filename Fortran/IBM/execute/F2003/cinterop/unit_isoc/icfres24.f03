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
          type(c_funptr) :: f(5)
          f = c_null_funptr
        end function f
      end module

      program icfres24
        use iso_c_binding
        use m
        type(c_funptr) :: cp(5)
        cp = f()
        if (c_associated(cp(3))) error stop 1
      end program icfres24
