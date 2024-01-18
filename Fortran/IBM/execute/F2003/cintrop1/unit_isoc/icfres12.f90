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
! %POSTCMD: ${TR_SRC}/icfres.sh 12
! %END
!**********************************************************************
!**********************************************************************
!*  =================================================================== 
!*  XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE NAME             : C_PTR and C_FUNPTR function results
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
      module m
        integer, target :: i
      end module m

      program icfres12
        use iso_c_binding
        use m
        type(c_ptr) :: cp(5)
        integer, pointer :: p1
        i = 42
        cp = f()
        call c_f_pointer(cp(3), p1)
        if (.not.c_associated(cp(3), c_loc(i))) error stop 1
        if (.not.c_associated(c_loc(i), cp(3))) error stop 6
        if (.not.c_associated(cp(3))) error stop 7
        if (.not.associated(p1)) error stop 10
        if (.not.associated(p1, i)) error stop 12
        if (p1 /= i) error stop 15
        if (p1 /= 42) error stop 17
        if (.not.c_associated(cp(3), c_loc(p1))) error stop 19
      contains
        function f()
          use iso_c_binding
          use m
          type(c_ptr) :: f(5)
          f = c_loc(i)
        end function f
      end program icfres12
