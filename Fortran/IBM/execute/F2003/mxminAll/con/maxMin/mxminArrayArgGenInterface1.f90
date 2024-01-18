!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with named constant as actual
!*                               argument to subprogram with generic
!*                               interface name the same as intrinsic
!*                               name
!*  (314919)
!* ===================================================================

  program mxminArrayArgGenInterface1

       intrinsic max

       interface max
          logical function maxlog(arg1, arg2)
                logical arg1, arg2
          end function
       end interface

       character*4 x1(4), y1(4)
       logical     x2, y2, z2

       parameter(x1 = "f_r")
       parameter(y1 = "gol")
       parameter(x2 = .true., y2 = .false.)

       if(any(max(x1, y1) .ne. "gol")) then
             error stop 1_4
       endif

       if(max(x2, y2) .neqv. .true. ) then
             error stop 2_4
       endif

  end program mxminArrayArgGenInterface1

       logical function maxlog(arg1, arg2)
             logical arg1, arg2
             maxlog = arg1
       end function

