!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with variable as actual
!*                               argument to subprogram with generic
!*                               interface name the same as intrinsic
!*                               name
!*  (314919)
!* ===================================================================

  program mxminvalVarGenInterface

       intrinsic maxval

       interface maxval
          logical function maxlog(arg1, arg2)
                logical arg1, arg2
          end function
       end interface

       character*4 x(2,4,6,8)
       logical     x2, y2

       x = "gol"
       x2 = .true.
       y2 = .false.

       if(maxval(x) .ne. "gol") error stop 1_4

       if(any(maxval(x, dim=2) .ne. "gol")) error stop 2_4

       if(any(maxval(x, dim=1, mask=.true.) .ne. "gol")) error stop 3_4

       if(maxval(x2, y2) .neqv. .true. ) then
             error stop 4_4
       endif

  end program mxminvalVarGenInterface

       logical function maxlog(arg1, arg2)
             logical arg1, arg2
             maxlog = arg1
       end function