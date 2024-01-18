!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with literal as actual
!*                               argument to subprogram. Using typeless,
!*                               byte or integer as DIM and MASK.
!* ===================================================================
@process intlog

  module ArgLiteralSub1
     contains
        subroutine sub2(arg)
           character(*), dimension(*) :: arg
           if(any(arg(1:1) .ne. "ddd")) error stop 1_4
           if(any(arg(2:2) .ne. "sss")) error stop 2_4
        end subroutine
        subroutine sub3(arg)
          character(*), dimension(*) :: arg
          if(any(ichar(arg(1:2)) .ne. 127)) error stop 3_4
        end subroutine

  end module ArgLiteralSub1

@process intlog
  program mxminvalLiteralArgSub1

    use ArgLiteralSub1

    interface
        function func1(carg, n)
          character(*),dimension(*) :: carg
          character(n),dimension(n) :: func1
        end function
    end interface

    character*2 v(2)
    byte     v1, v2(2,3)
    integer  v3, v4(2,3)

    v1 = 2
    v3 = 1
    v4 = 1
    v2 = .false.

    call sub2(maxval(reshape((/"ddd", "bbb", "aaa", "sss", "ddd", "bbb"/), (/2,3/)), dim= b"010"))

    call sub2(maxval(reshape((/"ddd", "bbb", "aaa", "sss", "ddd", "bbb"/), (/2,3/)), dim= Z"002" , mask = v4))

    call sub3(minval(reshape((/"ddd", "bbb", "aaa", "sss", "ddd", "bbb"/), (/2,3/)), dim= O"002", mask = v2))

    call sub1(minval((/"ddd", "bbb", "aaa", "ttt"/),dim= v3), 4)

    v = func1(maxval(reshape((/"zz", "gg", "aa", "ss", "dd","aa"/), (/2,3/)), dim= v1), 2)

    if(v(1) .ne. "zz" .or. v(2) .ne. "ss") error stop 4_4

    contains
         subroutine sub1(carg, iarg)
            character(*) :: carg
            integer iarg
            character*3  carray(iarg)

            carray = "xxx"
            carray(3) = "yyy"

            if(carg .ne. "aaa") error stop 5_4

            if(maxval(max(carray, carg, carray(1))) .ne. "yyy") then
                error stop 6_4
            endif

         end subroutine

  end program mxminvalLiteralArgSub1

  function func1(carg, n)
       character(*),dimension(*) :: carg
       character(n), dimension(n) :: func1
       do i = 1, n
           func1(i) = carg(i)
       end do
  end function
