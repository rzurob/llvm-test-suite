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
      program icfres14
        use iso_c_binding
        type(c_funptr) :: cp(5)
        cp = f()
        if (c_associated(cp(3))) error stop 1
      contains
        function f()
          use iso_c_binding
          type(c_funptr) :: f(5)
          f = c_null_funptr
        end function f
      end program icfres14
