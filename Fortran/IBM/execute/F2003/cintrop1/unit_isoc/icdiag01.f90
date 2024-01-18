!**********************************************************************
!*  ===================================================================
!*
!*                               ISO_C_BINDING module.
!*
!*  DATE                       : June 16, 2003
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  06/16/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
      program icdiag01
        use iso_c_binding
        logical :: l
        type(c_ptr) :: a, b, c
        integer :: d
        type t
          integer i
        end type
        type(t) :: e
        l = c_associated()
        l = c_associated(a, b, c)
        l = c_associated(d)
        l = c_associated(e)
      end program icdiag01
