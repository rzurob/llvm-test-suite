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
      program iccomp36
        use iso_c_binding
        type :: t
          type(c_funptr) :: a(5) = (/c_null_funptr,c_null_funptr,            &
     &                   c_null_funptr, c_null_funptr, c_null_funptr/)
        end type
        type(t) :: x
        if (c_associated(x%a(3))) error stop 1
        if (c_associated(x%a(3), c_null_funptr)) error stop 2
        if (c_associated(c_null_funptr, x%a(3))) error stop 3
        if (c_associated(x%a(3), x%a(3))) error stop 4
      end program iccomp36
