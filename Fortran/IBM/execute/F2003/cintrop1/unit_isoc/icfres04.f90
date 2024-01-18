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
! %POSTCMD: ${TR_SRC}/icfres.sh 04
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C_PTR and C_FUNPTR function results
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
      program icfres04
        use iso_c_binding
        interface
          function f()
            use iso_c_binding
            type(c_funptr) :: f(5)
          end function
        end interface
        type(c_funptr) :: cp(5)
        cp = f()
        if (c_associated(cp(3))) error stop 1
      end program icfres04

      function f()
        use iso_c_binding
        type(c_funptr) :: f(5)
        f = c_null_funptr
      end function f
