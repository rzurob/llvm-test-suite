!**********************************************************************
!*  ===================================================================
!*
!*                               the POINTER and ALLOCATABLE attributes
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
      program icattr13
        use iso_c_binding
        type d
          type(c_funptr), pointer :: cp
        end type
        type(d) :: z
        integer, target :: t
        integer, pointer :: p
        allocate(z%cp)
        z%cp = c_null_funptr
        if (c_associated(z%cp)) error stop 1
        if (c_associated(z%cp, c_null_funptr)) error stop 3
        deallocate(z%cp)
      end program icattr13
