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
! %POSTCMD: dcomp icdiag03.f
! %END
!**********************************************************************
!**********************************************************************
!*  =================================================================== 
!*  XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE NAME             : icdiag03.f
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
      @PROCESS ZEROSIZE
      program icdiag03
        use iso_c_binding
        type(c_ptr) :: cp
        type(c_funptr) :: fp
        integer :: a
        external :: b
        character(2), target :: c(5)
        byte, pointer :: d(:)
        integer :: f(0)
        integer, target :: g(5)
        interface
          subroutine sub
          end subroutine sub
        end interface
        cp = c_loc(a)
        cp = c_loc(b)
        cp = c_loc(c)
        cp = c_loc(d)
        cp = c_loc()
        cp = c_loc(a, b)
        cp = c_loc(g(2:1))
        fp = c_funloc(b)
        fp = c_funloc(sub)
        fp = c_funloc(a)
        fp = c_funloc()
        fp = c_funloc(a, b)
      end program icdiag03
