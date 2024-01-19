! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : Save and Restore
!*  SECONDARY FUNCTIONS TESTED :
!*
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
