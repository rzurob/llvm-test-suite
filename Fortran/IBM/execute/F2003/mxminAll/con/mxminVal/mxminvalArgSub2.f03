!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with named constant as actual
!*                               argument to subprogram with argument keyword.
!*                               Dummy argument  with inherited length.
!*                               No DIM or MASK specified.
!* ===================================================================

  program mxminvalArgSub2

    interface
        subroutine sub1(arg1, arg2)
            character(*):: arg1
            character(*):: arg2
        end subroutine
    end interface

    character(len=4), parameter :: x(4) =(/"a_*n","b\x\x ","c__ "," g__"/)
    character(len=1), parameter :: y(4) =(/"a","b","c","g"/)

    call sub1(arg2= maxval(x), arg1 = minval(y))

  end program mxminvalArgSub2

  subroutine sub1(a, b)
       character(*) a
       character(*) b
       if(a .ne. "a") error stop 1_4
       if(b .ne. "c__ ") error stop 2_4
  end subroutine
