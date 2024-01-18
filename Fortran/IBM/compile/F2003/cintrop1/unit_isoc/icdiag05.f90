!**********************************************************************
!*  ===================================================================
!*
!*                               ISO_C_BINDING module.
!*
!*  DATE                       : June 17, 2003
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  06/17/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
      subroutine sub(a, b, n)
        use iso_c_binding
        integer :: n
        character(n), target :: a(5)
        character(*), target :: b(5)
        type(c_ptr) :: p
        p = c_loc(a)
        p = c_loc(a(3))
        p = c_loc(b)
        p = c_loc(b(3))
      end subroutine sub
