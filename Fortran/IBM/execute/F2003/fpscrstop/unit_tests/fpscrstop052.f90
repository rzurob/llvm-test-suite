!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 21, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : When STOP stmt is executed and a floating-
!*                               point exception is occurred, an informational
!*                               message should be displayed indicating which
!*                               FPSCR exception flags are set.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=stopexcept
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : Miscellaneous: Make sure when STOP
!*                               is reached and a FPSCR exception is
!*                               signaling, no recursive i/o errors
!*                               are encountered. ( This is useful to
!*                               test if later on the RTE implementation
!*                               of this feature involvs Fortran code ).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_exceptions
      implicit none

      print*, f1()

      stop "End Main"


      contains
      function f1()
        integer :: f1
        call ieee_set_flag(ieee_all, .true.)
        f1 = 3
        stop "End f1"
      end function

      end
