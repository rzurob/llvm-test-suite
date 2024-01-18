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
!*  DESCRIPTION                : MAX/MIN with literal as actual
!*                               argument to subprogram - with optional
!*                               argument as function return result.
!*                               Use literal substring as max/min argument
!*
!* ===================================================================

  module subLitArg2
     contains
        subroutine sub2(arg)
           character(*) arg(2,3)
           character*4  carg(2,3)
           if(len(arg) .ne. 3) then
               error stop 7_4
           endif
           if(any(arg .ne. "gko")) then
               error stop 8_4
           end if
           carg = "fko"
           if(any(min(arg, carg, max(carg, arg), carg) .ne. "fko")) then
                error stop 9_4
           endif
        end subroutine
  end module subLitArg2 

  program mxminLiteralArrArgSub2 
    
    use subLitArg2 

    interface
        function func1(carg, n)
          character(*), dimension(*) :: carg 
          character(n), dimension(2*n) :: func1
        end function 
    end interface

    ! max/min as its own argument

    call sub2(max(reshape((/"gko","gko","gko","gko","gko","gko"/), (/2,3/)), reshape((/"fgh","fgh","fgh","fgh","fgh","fgh"/), (/2,3/)), min(reshape((/"gko","gko","gko","gko","gko","gko"/), (/2,3/)), reshape((/"fgh","fgh","fgh","fgh","fgh","fgh"/), (/2,3/)), reshape((/"gko","gko","gko","gko","gko","gko"/), (/2,3/))), max(reshape((/"gko","gko","gko","gko","gko","gko"/), (/2,3/)), "aaa"), reshape((/"gko","gko","gko","gko","gko","gko"/), (/2,3/))))

    if(any(LLT(min((/"ddd", "ddd","ddd","ddd","ddd","ddd"/), "bbb"), max((/"ddd", "ddd","ddd","ddd","ddd","ddd"/), "bbb")) .neqv. .true.)) error stop 1_4

    ! literal substring

    if(any(max((/"asdddgf"(3:5), "asdddgf"(3:5),"asdddgf"(3:5),"asdddgf"(3:5),"asdddgf"(3:5),"asdddgf"(3:5)/), "bbb") .ne. "ddd")) error stop 2_4


    ! function return as argument

    call sub1(max((/"ddd", "ddd","ddd","ddd","ddd","ddd"/),"bbb", func1(max(reshape((/"gko","gko","gko","gko","gko","gko"/), (/2,3/)), reshape((/"fgh","fgh","fgh","fgh","fgh","fgh"/), (/2,3/))), 3)), 6)


    if(any(func1(max(reshape((/"gko","gko","gko","gko","gko","gko"/), (/2,3/)), reshape((/"fgh","fgh","fgh","fgh","fgh","fgh"/), (/2,3/))),3) .ne. "gko")) error stop 4_4

    contains
         subroutine sub1(carg, iarg)
            character(*), dimension(*) :: carg
            integer iarg
            character*3  carray(iarg)
            character*3, parameter::darg(6) = "zzz" 
             
            carray = "xxx"

            if(any(max(carg(1:6), carray, darg) .ne. "zzz")) then
                   error stop 5_4
            endif
            if(any(min(carg(1:6), carray, darg) .ne. "gko")) then
                   error stop 6_4
            endif

         end subroutine 

  end program mxminLiteralArrArgSub2 

  function func1(carg, n)
       character(*), dimension(*) :: carg 
       character(n), dimension(2*n) :: func1
       do i = 1, 2*n
           func1(i) = carg(i)
       end do
  end function
   
