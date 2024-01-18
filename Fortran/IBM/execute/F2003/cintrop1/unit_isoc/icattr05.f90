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
      program icattr05
        use iso_c_binding
        type(c_ptr), allocatable :: cp
        integer, target :: t
        integer, pointer :: p
        allocate(cp)
        cp = c_loc(t)
        t = 42
        call c_f_pointer(cp, p)
        if (.not.c_associated(cp)) error stop 1
        if (.not.c_associated(cp, c_loc(t))) error stop 2
        if (c_associated(cp, c_null_ptr)) error stop 3
        if (.not.c_associated(cp, c_loc(p))) error stop 4
        if (p /= 42) error stop 5
        if (.not.associated(p, t)) error stop 6
        deallocate(cp)
      end program icattr05
