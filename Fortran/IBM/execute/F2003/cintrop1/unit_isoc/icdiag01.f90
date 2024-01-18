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
! %POSTCMD: dcomp icdiag01.f
! %END
!**********************************************************************
!**********************************************************************
!*  =================================================================== 
!*  XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE NAME             : icdiag01.f
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
      program icdiag01
        use iso_c_binding
        logical :: l
        type(c_ptr) :: a, b, c
        integer :: d
        type t
          integer i
        end type
        type(t) :: e
        l = c_associated()
        l = c_associated(a, b, c)
        l = c_associated(d)
        l = c_associated(e)
      end program icdiag01
