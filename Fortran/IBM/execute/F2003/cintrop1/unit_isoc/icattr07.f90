!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: icattr07.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!**********************************************************************
!*  =================================================================== 
!*  XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE NAME             : C_PTR and C_FUNPTR objects with
!*                               the POINTER and ALLOCATABLE attributes
!*  TEST CASE TITLE            : Functional testing of the
!*                               ISO_C_BINDING module.
!*                                                                     
!*  PROGRAMMER                 : Rob James
!*  DATE                       : June 12, 2003
!*  ORIGIN                     : XL Fortran Compiler Development
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
      program icattr07
        use iso_c_binding
        type d
          type(c_ptr), allocatable :: cp
        end type
        type(d) :: z
        integer, target :: t
        integer, pointer :: p
        allocate(z%cp)
        z%cp = c_loc(t)
        t = 42
        call c_f_pointer(z%cp, p)
        if (.not.c_associated(z%cp)) error stop 1
        if (.not.c_associated(z%cp, c_loc(t))) error stop 2
        if (c_associated(z%cp, c_null_ptr)) error stop 3
        if (.not.c_associated(z%cp, c_loc(p))) error stop 4
        if (p /= 42) error stop 5
        if (.not.associated(p, t)) error stop 6
        deallocate(z%cp)
      end program icattr07
