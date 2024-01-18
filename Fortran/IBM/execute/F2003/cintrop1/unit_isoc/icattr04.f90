!#######################################################################
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: icattr04.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C_PTR and C_FUNPTR objects with
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
      program icattr04
        use iso_c_binding
        type d
          type(c_ptr), pointer :: cp(:)
        end type
        type(d) :: z
        integer, target :: t
        integer, pointer :: p
        allocate(z%cp(5))
        z%cp = c_loc(t)
        t = 42
        call c_f_pointer(z%cp(3), p)
        if (.not.c_associated(z%cp(3))) error stop 1
        if (.not.c_associated(z%cp(3), c_loc(t))) error stop 2
        if (c_associated(z%cp(3), c_null_ptr)) error stop 3
        if (.not.c_associated(z%cp(3), c_loc(p))) error stop 4
        if (p /= 42) error stop 5
        if (.not.associated(p, t)) error stop 6
        deallocate(z%cp)
      end program icattr04
