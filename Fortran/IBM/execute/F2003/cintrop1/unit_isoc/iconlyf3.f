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
! %POSTCMD: ${TR_SRC}/iconly.sh 3
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Testing USE, ONLY with C_LOC and
!*                               C_FUNLOC
!*                               ISO_C_BINDING module.
!*
!*  DATE                       : June 16, 2003
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
      program iconlyf3
        use iso_c_binding, only: c_loc, c_null_ptr, c_bool
        logical(c_bool), external :: foo
        if (.not. foo(c_null_ptr)) error stop 1
      end program iconlyf3
