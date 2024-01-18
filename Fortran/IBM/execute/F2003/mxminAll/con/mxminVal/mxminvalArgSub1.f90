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
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with named constant as actual
!*                               argument to subprogram. No DIM or MASK
!*                               specified.
!* ===================================================================

  module ArgSub1
     contains
        subroutine sub2(arg)
           character(*) arg
           if(len(arg) .ne. 3) then
               error stop 7_4
           endif
           if(arg .ne. "sss") error stop 1_4
        end subroutine
  end module ArgSub1 

  program mxminvalArgSub1 
    
    use ArgSub1 

    interface
        function func1(carg, n)
          character(*) :: carg 
          character(n) :: func1
        end function 
    end interface

    character*3, parameter :: x(4) = (/"ddd", "bbb", "aaa", "ttt"/)
    character*3, parameter :: z1(2,3) = reshape((/"ddd", "bbb", "aaa", "sss", "ddd", "bbb"/), (/2,3/))
    character*3, parameter :: z2(2,3) = reshape((/"zzz", "ggg", "aaa", "sss", "ddd", "aaa"/), (/2,3/))

    call sub2(maxval(z1))

    call sub1(minval(x), 4)

    if(func1(maxval(z2),3) .ne. "zzz") error stop 3_4 

    contains
         subroutine sub1(carg, iarg)
            character(*) :: carg
            integer iarg
            character*3  carray(iarg)
             
            carray = "xxx"
            carray(3) = "yyy"
          
            if(carg .ne. "aaa") error stop 2_4

            if(maxval(max(carray, carg, carray(1))) .ne. "yyy") then
                error stop 4_4
            endif

         end subroutine 

  end program mxminvalArgSub1 

  function func1(carg, n)
       character(*) :: carg
       character(n) :: func1
       func1 = carg
  end function

