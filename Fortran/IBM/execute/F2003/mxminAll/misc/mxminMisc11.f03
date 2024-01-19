!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX*/MIN* as actual argument passed to
!*                               subprogram with  dummy argument as
!*                               assumed shape array
!*
!*                               different length dummy argument as argument
!*                               to MAX/MIN
!*
!*                               MAX*/MIN* with DIM and MASK
!* ===================================================================

program mxminMisc11

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

   call sub1(maxval(x, dim=1, mask=.true.))

   call sub2(minloc(x, dim = 2, mask = .true.))

   call sub3(max(x, y))

end program mxminMisc11

   subroutine sub1(arg)
       character*3 arg(3:, :, 10:, :)
       integer v(3)
       if(any(arg(3:, :, 10:, :) .ne. 'aaa')) then
             error stop 1_4
       endif
       arg(4, :, :, :) ="___"
       if(any(minval(arg, dim =1,mask=.true.) .ne. "___")) then
             error stop 2_4
       endif
       v = shape(minval(arg, dim=1, mask=.true.))
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
      character*3 :: varg1(3,4,5,6,7)
      character*5 :: varg2(3,4,5,6,7)
      if(any(arg(:, :, 3:, 4:, :9) .ne. "bbb")) then
             error stop 5_4
      endif
      if(any(maxloc(arg, dim=1, mask = .true.) .ne. 1)) then
            error stop 6_4
      endif
      varg1 = "ggg"
      varg2 = "zzzzz"
      if(len(max(varg1, varg2, arg)) .ne. 5) then
             error stop 7_4
      endif
      if(any(max(varg1, varg2, arg) .ne. "zzzzz")) then
            error stop 8_4
      endif
   end subroutine

