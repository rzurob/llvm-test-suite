!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL as expression with named
!*                               constant as actual argument to elemental
!*                               subprogram with defined assignment.
!*
!* ===================================================================

  program mxminvalDefAssign

       interface assignment(=)
          elemental subroutine char_to_integer(arg1, arg2)
                integer, intent(out) :: arg1
                character*1, intent(in) :: arg2
          end subroutine
       end interface

       character*1 x(2,3,4,5,6), y(2,3,4,5,6)
       integer     z(2, 3,5,6)

       parameter(x = "f")
       parameter(y = "g")

       z = maxval(x, dim=3, mask=.true.)

       if(any(z .ne. 102)) then
           error stop 1_4
       endif

       z = minval(y, dim=3, mask=.true.)

       if(any(z .ne. 103)) then
           error stop 2_4
       endif

  end program mxminvalDefAssign

       elemental subroutine char_to_integer(arg1, arg2)
             integer, intent(out) :: arg1
             character*1, intent(in) :: arg2
             character*1 arg3
             arg1 = ichar(max(arg2, min(arg3, arg2)))
       end subroutine

