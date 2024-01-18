!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with literal as actual
!*                               argument to subprogram with argument keyword.
!*                               Dummy argument  with inherited length.
!* ===================================================================
@process intlog

  program mxminvalLiteralArgSub2

    interface
        subroutine sub1(arg1, arg2)
            character(*):: arg1
            character(*):: arg2
        end subroutine
    end interface

    integer v(4)

    v = .true.

    call sub1(arg2= maxval((/"a_*n","b\x\x ","c__ "," g__"/), mask=.true.), arg1 = minval((/"a","b","c","g"/), dim=b"001", mask=v))

  end program mxminvalLiteralArgSub2

  subroutine sub1(a, b)
       character(*) a
       character(*) b
       if(a .ne. "a") error stop 1_4
       if(b .ne. "c__ ") error stop 2_4
  end subroutine
