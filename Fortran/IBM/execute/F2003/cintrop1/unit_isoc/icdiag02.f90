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
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp icdiag02.f
! %END
!**********************************************************************
!**********************************************************************
!*  =================================================================== 
!*  XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE NAME             : icdiag02.f
!*  TEST CASE TITLE            : Diagnostic testing of the
!*                               ISO_C_BINDING module.
!*                                                                     
!*  PROGRAMMER                 : Rob James
!*  DATE                       : June 16, 2003
!*  ORIGIN                     : XL Fortran Compiler Development
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
      program icdiag02
        use iso_c_binding
        type(c_ptr) :: a
        integer, pointer :: b
        integer :: c(5)
        integer :: d
        type(c_funptr) :: e
        type dt
          integer i
        end type
        type(dt) :: f
        integer, pointer :: g(:)
        integer :: h(5, 5)
        character :: i(5)
        call c_f_pointer()
        call c_f_pointer(a)
        call c_f_pointer(a, b, c, d)
        call c_f_pointer(e, b)
        call c_f_pointer(d, b)
        call c_f_pointer(f, b)
        call c_f_pointer(a, d)
        call c_f_pointer(a, b, d)
        call c_f_pointer(a, g)
        call c_f_pointer(a, g, c)
        call c_f_pointer(a, g, h)
        call c_f_pointer(a, b, i)
      end program icdiag02
