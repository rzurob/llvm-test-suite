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
      program icattr12
        use iso_c_binding
        type(c_funptr), pointer :: cp(:)
        integer, target :: t
        integer, pointer :: p
        allocate(cp(2))
        cp = (/c_null_funptr,c_null_funptr/)
        if (c_associated(cp(1))) error stop 1
        if (c_associated(cp(1), c_null_funptr)) error stop 3
        deallocate(cp)
      end program icattr12
