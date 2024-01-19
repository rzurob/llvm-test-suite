!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN as expression with literal as
!*                               actual argument to elemental subprogram with
!*                               defined assignment
!*
!* ===================================================================

  program mxminLiteralArrDefAssign

       interface assignment(=)
          elemental subroutine char_to_integer(arg1, arg2)
                integer, intent(out) :: arg1
                character*1, intent(in) :: arg2
          end subroutine
       end interface

       integer     z(3)

       z = max((/"f", "f", "f"/), (/"g", "g", "g"/), "f")

       if(any(z .ne. 122)) then
           error stop 1_4
       endif

       z = min((/"f", "f", "f"/), (/"g", "g", "g"/), max((/"f", "f", "f"/), (/"g", "g", "g"/)))

       if(any(z .ne. 122)) then
           error stop 2_4
       endif

  end program mxminLiteralArrDefAssign

       elemental subroutine char_to_integer(arg1, arg2)
             integer, intent(out) :: arg1
             character*1, intent(in) :: arg2
             character*1 arg3
             arg3 = "z"
             arg1 = ichar(max(arg2, min(arg3, arg2), arg3))
       end subroutine

