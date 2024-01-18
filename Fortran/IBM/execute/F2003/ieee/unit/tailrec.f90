! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: tailrec.f
! %VERIFY: tailrec.out:tailrec.vf
! %STDIN:
! %STDOUT: tailrec.out
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
!*  DESCRIPTION                : test that tail recursion does not
!*                               cause save and restore problems
!*                               This test case used to fail because
!*                               of a problem in tobey.  Keep it
!*                               unchanged to verify that the tobey
!*                               problem hasn't reappeared.
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
                recursive real*8 function xxxx(x,y)
                  use ieee_exceptions
                  real(8) :: x,y
                  logical :: val

                  if (y == -2d0) then
                    xxxx = y
                    return
                  endif

                 call ieee_get_flag(ieee_divide_by_zero, val)
                 if (val) then
                   print *, "Failed"
                   xxxx = -10.0
                   return
                 end if

                 x = x / y

                 xxxx = xxxx(x,y-1d0)
               end function

               real(8) :: y
               y =  xxxx(2d0,1d0)
               print *, y
               end
