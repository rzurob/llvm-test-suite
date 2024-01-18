! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: symflag7.f
! %VERIFY: symflag7.out:symflag7.vf
! %STDIN:
! %STDOUT: symflag7.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Rafik Zurob
!*  DATE                       : March, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Save and Restore 
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test Save and restore
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      ! The whole program (_main) should be wrapped in
      ! prologue/epilogue code.  (don't know how useful
      ! this is.)


!     _main
!     _main has the bit set
!     The call to ieee_set_flag has the bit set.
      use ieee_exceptions
      logical :: val(5)
      call ieee_set_flag(ieee_invalid, .true.)
      call ieee_get_flag(ieee_all, val)
      print *, val
      end
