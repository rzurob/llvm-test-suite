!#######################################################################
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iccomp05.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C_PTR and C_FUNPTR components
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
      program iccomp05
        use iso_c_binding
        type :: t
          type(c_funptr) :: a
        end type
        type(t) :: x
        x = t(c_null_funptr)
        if (c_associated(x%a)) error stop 1
        if (c_associated(x%a, c_null_funptr)) error stop 2
        if (c_associated(c_null_funptr, x%a)) error stop 3
        if (c_associated(x%a, x%a)) error stop 4
      end program iccomp05
