!**********************************************************************
!*  ===================================================================
!*
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
      program iconlyf1
        use iso_c_binding, only: c_loc, c_bool
        logical(c_bool), external :: foo
        integer, target :: i
        i = 42
        if (.not. foo(c_loc(i))) error stop 1
      end program iconlyf1