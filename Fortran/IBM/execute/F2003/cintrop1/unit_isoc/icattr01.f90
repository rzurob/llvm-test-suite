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
! %GROUP: icattr01.f
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
      program icattr01
        use iso_c_binding
        type(c_ptr), pointer :: cp
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
      end program icattr01
