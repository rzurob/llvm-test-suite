!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL as expression with variable as
!*                               actual argument to elemental subprogram with
!*                               defined assignment
!* ===================================================================

  program mxminVarDefAssign

       interface assignment(=)
          elemental subroutine char_to_integer(arg1, arg2)
                integer, intent(out) :: arg1
                character*1, intent(in) :: arg2
          end subroutine
       end interface

       character*1 x(2,3,4,5,6), y(2,3,4,5,6)
       integer     z(2,3,4,5,6)

       x = "f"
       y = "g"

       z = max(x, y, min(x, y), y)

       if(any(z .ne. 122)) then
           error stop 1_4
       endif

       z = min(x, y, max(x, y), y)

       if(any(z .ne. 122)) then
           error stop 2_4
       endif

  end program mxminVarDefAssign

       elemental subroutine char_to_integer(arg1, arg2)
             integer, intent(out) :: arg1
             character*1, intent(in) :: arg2
             character*1 arg3
             arg3 = "z"
             arg1 = ichar(max(arg2, min(arg3, arg2), arg3))
       end subroutine
