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
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAXVAL/MINVAL as expression with literal 
!*                               as actual argument to elemental subprogram
!*                               with defined assignment.
!*                             
!* ===================================================================

  program mxminvalLiteralDefAssign 

       interface assignment(=) 
          elemental subroutine char_to_integer(arg1, arg2)
                integer, intent(out) :: arg1 
                character*1, intent(in) :: arg2 
          end subroutine 
       end interface

       character*1 x(2,3), y(2,3)
       integer     z(2)

       z = maxval(reshape((/"a", "b", "c", "d", "e", "f"/), (/2,3/)), dim=2, mask=.true.)

       if(z(2) .ne. 102 .or. z(1) .ne. 101) then
           error stop 1_4
       endif

       z = minval(reshape((/"g", "h", "i", "j", "k", "z"/), (/2,3/)), dim=2, mask=.true.) 

       if(z(2) .ne. 104 .or. z(1) .ne. 103) then
           error stop 2_4
       endif

  end program mxminvalLiteralDefAssign 

       elemental subroutine char_to_integer(arg1, arg2)
             integer, intent(out) :: arg1
             character*1, intent(in) :: arg2
             character*1 arg3
             arg1 = ichar(max(arg2, min(arg3, arg2)))
       end subroutine


