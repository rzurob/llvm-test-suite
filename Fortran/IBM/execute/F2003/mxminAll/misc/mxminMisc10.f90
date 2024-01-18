!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX*/MIN* as actual argument passed to
!*                               subprogram with  dummy argument as
!*                               assumed shape array
!*
!*                               dummy argument as argument to MAX*/MIN*
!*
!*                               MAX*/MIN* with DIM
!* ===================================================================

program mxminMisc10 

   interface
        subroutine sub1(arg)
            character*3 arg(3:, :, 10:, :)
        end subroutine

        subroutine sub2(arg)
            integer :: arg(:, 2:, 3:, 4:)
        end subroutine

        subroutine sub3(arg)
            character*3 :: arg(:, :, 3:, 4:, 9:)
        end subroutine
   end interface

   character*3 x(3, 4, 5, 6, 7), y(3, 4, 5, 6, 7)

   x = "aaa"

   y = "bbb"

   call sub1(maxval(x, dim=1))

   call sub2(minloc(x, dim = 2))

   call sub3(max(x, y))

end program mxminMisc10 

   subroutine sub1(arg)
       character*3 arg(3:, :, 10:, :)
       integer v(3)
       if(any(arg(3:, :, 10:, :) .ne. 'aaa')) then
             error stop 1_4
       endif
       arg(4, :, :, :) ="___"
       if(any(minval(arg, dim =1) .ne. "___")) then
             error stop 2_4
       endif
       v = shape(minval(arg, dim=1))
       if(v(1) .ne. 5 .or. v(2) .ne. 6 .or. v(3) .ne. 7) then
             error stop 3_4
       endif 
   end subroutine

   subroutine sub2(arg)
       integer :: arg(:, 2:, 3:, 4:)
       if(any(arg(:, 2:, 3:, 4:) .ne. 1)) then
            error stop 4_4
       endif
   end subroutine

   subroutine sub3(arg)
      character*3 :: arg(:, :, 3:, 4:, 9:)
      character*3 :: varg(3,4,5,6,7)
      if(any(arg(:, :, 3:, 4:, :9) .ne. "bbb")) then
             error stop 5_4
      endif
      if(any(maxloc(arg, dim=1) .ne. 1)) then
            error stop 6_4
      endif
      varg = "zzz"
      if(any(max(varg, arg) .ne. "zzz")) then
            error stop 7_4
      endif
   end subroutine
