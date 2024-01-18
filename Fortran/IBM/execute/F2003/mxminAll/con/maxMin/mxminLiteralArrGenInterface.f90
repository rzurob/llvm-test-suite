!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with literal as actual
!*                               argument to subprogram with generic
!*                               interface name the same as intrinsic
!*                               name
!*  (314919)
!* ===================================================================

  program mxminLiteralArrGenInterface

       intrinsic max

       interface max
          logical function maxlog(arg1, arg2)
                logical arg1, arg2
          end function
       end interface

       logical     x2, y2, z2

       parameter(x2 = .true., y2 = .false.)

       if(any(max((/"f_r", "f_r", "f_r"/), (/"gol","gol","gol"/)) .ne. "gol")) then
             error stop 1_4
       endif

       if(max(x2, y2) .neqv. .true. ) then
             error stop 2_4
       endif

  end program mxminLiteralArrGenInterface

       logical function maxlog(arg1, arg2)
             logical arg1, arg2
             maxlog = arg1
       end function

