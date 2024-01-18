!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX/MIN with variable as actual argument
!*                               to subprogram with generic  interface
!*                               name the same as intrinsic name
!*  (314919)                                 
!* ===================================================================

  program mxminVarGenInterface

       intrinsic min

       interface min
          logical function minlog(arg1, arg2)
                logical arg1, arg2
          end function
       end interface

       character*4 x1(4), y1(4)
       logical     x2, y2, z2

       x1 = "f_r"
       y1 = "gol"
       x2 = .true. 
       y2 = .false.

       if(any(min(x1, y1) .ne. "f_r")) then
             error stop 1_4
       endif

       if(min(x2, y2) .neqv. .true. ) then
             error stop 2_4
       endif 

  end program mxminVarGenInterface 

       logical function minlog(arg1, arg2)
             logical arg1, arg2
             minlog = arg1
       end function


