!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with named constant as actual
!*                               argument to subprogram with argument keyword.
!* ===================================================================

  program mxminLocVarArrArgSub2

    interface
        subroutine sub1(arg1, arg2, arg3)
            integer :: arg1(2)
            integer :: arg2(3)
            integer :: arg3
        end subroutine
    end interface

    logical :: m(2,3) = .true.
    character*3 :: x(5) = (/(char(i+70), i = 1,10,2)/)
    character*3 :: y(2,3) = reshape((/"bbb", "aaa", "ccc", "ddd","fff", "ggg"/),(/2,3/))

    call sub1(arg3 = maxloc(x,dim=1), arg2 = maxloc(y,dim=1), arg1 = minloc(y, dim=2, mask=m))

  end program mxminLocVarArrArgSub2

  subroutine sub1(arg1, arg2, arg3)
       integer :: arg1(2)
       integer :: arg2(3)
       integer :: arg3
       if(arg1(1) .ne. 1 .or. arg1(2) .ne. 1) error stop 1_4
       if(arg2(1) .ne. 1 .or. arg2(2) .ne. 2 .or. arg2(3) .ne. 2) then
            error stop 2_4
       endif
       if(arg3 .ne. 5) error stop 3_4
  end subroutine
