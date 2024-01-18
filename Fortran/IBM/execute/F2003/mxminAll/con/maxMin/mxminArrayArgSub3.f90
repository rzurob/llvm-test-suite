!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with named constant as actual
!*                               argument to subprogram with argument keyword
!*                               dummy argument  with inherited length
!*
!* ===================================================================

  program mxminArrayArgSub3

    interface
        subroutine sub1(arg1, arg2, arg3)
            character(*):: arg1(2,3)
            character(*):: arg2(2,3)
            character(*):: arg3(2,3)
        end subroutine
    end interface

    character(len=4) a(2,3)
    character(len=3) b(2,3)
    character(len=8) c(2,3)

    parameter(a = "a_*n")
    parameter(b = "b\x\x")
    parameter(c = "c__")

    call sub1(arg3= max(a, b, a), arg1 = min(b, c, a), arg2 = max(a,c))

  end program mxminArrayArgSub3

  subroutine sub1(x, y, z)
       character(*) x(2,3)
       character(*) y(2,3)
       character(*) z(2,3)
       if(len(x) .ne. 8) then
           error stop 1_4
       endif
       if(len(y) .ne. 8) then
           error stop 2_4
       endif
       if(len(z) .ne. 4) then
           error stop 3_4
       endif
       if(any(max(x, y, z) .ne. "c__     ")) then
            error stop 4_4
       endif
       if(len(min(x, y, z)) .ne. 8) then
           error stop 5_4
       endif
       if(any(min(x, y, z) .ne. "a_*n    ")) then
           error stop 6_4
       endif
  end subroutine
