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
! %POSTCMD: ${TR_SRC}/iconly.sh 2
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
      program iconlyf2
        use iso_c_binding, only: c_funloc, c_null_funptr, c_bool
        logical(c_bool), external :: foo
        if (.not. foo(c_null_funptr)) error stop 1
      end program iconlyf2
