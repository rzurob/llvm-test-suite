!**********************************************************************
!*  ===================================================================
!*
!*                               ISO_C_BINDING module.
!*
!*  DATE                       : May 29, 2003
!*
!*  DESCRIPTION                : Declaring derived types with C_PTR
!*                               and C_FUNPTR components.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  05/29/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
      program iccomp01
        use iso_c_binding
        type :: t
          type(c_ptr) :: a
        end type
        type(t) :: x
        integer, target :: targ
        integer, pointer :: ptr1, ptr2
        type(c_ptr) :: cp
        x%a = C_NULL_PTR
        if (c_associated(x%a)) error stop 1
        if (c_associated(x%a, c_null_ptr)) error stop 2
        if (c_associated(c_null_ptr, x%a)) error stop 3
        if (c_associated(x%a, x%a)) error stop 4
        x%a = c_loc(targ)
        if (.not. c_associated(x%a)) error stop 5
        if (c_associated(x%a, c_null_ptr)) error stop 6
        if (c_associated(c_null_ptr)) error stop 7
        if (.not. c_associated(x%a, x%a)) error stop 8
        cp = c_loc(targ)
        if (.not. c_associated(x%a, cp)) error stop 9
        if (.not. c_associated(cp, x%a)) error stop 10
        call c_f_pointer(x%a, ptr1)
        call c_f_pointer(cp, ptr2)
        if (.not. associated(ptr1)) error stop 11
        if (.not. associated(ptr1, ptr2)) error stop 12
        if (.not. associated(ptr2, ptr1)) error stop 13
        if (.not. associated(ptr1, targ)) error stop 14
      end program iccomp01