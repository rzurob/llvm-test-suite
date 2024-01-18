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
!*  DESCRIPTION                : MAXVAL/MINVAL with named constant as actual
!*                               argument to subprogram. Using typeless, 
!*                               byte or integer as DIM and MASK. 
!* ===================================================================
@process intlog

  module ArgSub4
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

  end module ArgSub4

@process intlog
  program mxminvalArgSub4
    
    use ArgSub4

    interface
        function func1(carg, n)
          character(*),dimension(*) :: carg 
          character(n),dimension(n) :: func1
        end function 
    end interface

    character*3, parameter :: x(4) = (/"ddd", "bbb", "aaa", "ttt"/)
    character*3, parameter :: z1(2,3) = reshape((/"ddd", "bbb", "aaa", "sss", "ddd", "bbb"/), (/2,3/))
    character*2, parameter :: z2(2,3) = reshape((/"zz", "gg", "aa", "ss", "dd", "aa"/), (/2,3/))
    character*2 v(2)

    byte     v1, v2(2,3)
    integer  v3, v4(2,3)

    v1 = 2
    v3 = 1
    v4 = 1
    v2 = .false.

    call sub2(maxval(z1, dim= b"010"))

    call sub2(maxval(z1, dim= Z"002" , mask = v4))

    call sub3(minval(z1, dim= O"002", mask = v2))

    call sub1(minval(x,dim= v3), 4)

    v = func1(maxval(z2, dim= v1), 2)

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

  end program mxminvalArgSub4

  function func1(carg, n)
       character(*),dimension(*) :: carg
       character(n), dimension(n) :: func1
       do i = 1, n
           func1(i) = carg(i)
       end do
  end function
