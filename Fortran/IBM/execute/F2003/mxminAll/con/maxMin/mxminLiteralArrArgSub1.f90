!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with literal as actual
!*                               argument to subprogram
!*
!*                               inside sub. argument of max/min is assumed
!*                               -size array, automatic array and assumed
!*                               length array
!*
!* ===================================================================

  module LiteralsubArg
     contains
        subroutine sub2(arg)
           character(*) arg(3)
           character*4  carg(3)
           if(len(arg) .ne. 3) then
               error stop 7_4
           endif
           if(any(arg .ne. "gko")) then
               error stop 8_4
           end if
           carg = "fko"
           if(any(min(arg, carg) .ne. "fko")) then
                error stop 9_4
           endif
        end subroutine
  end module LiteralsubArg

  program mxminLiteralArrArgSub1

    use LiteralsubArg

    interface
        function func1(carg, n)
          character(*), dimension(*) :: carg
          character(n), dimension(2*n) :: func1
        end function
    end interface

    character*3 x(2), z1(4) , z2(4)
    character*3 y
    parameter(x = "ddd", y = "bbb", z1="gko", z2="fgh")

    call sub2(max((/1_"gko", 1_"gko", 1_"gko", 1_"gko"/), (/"fgh", "fgh", "fgh", "fgh"/)))

    if(any(LLT(min((/"ddd", "ddd"/), "bbb"), max((/"ddd", "ddd"/), "bbb")) .neqv. .true.))then
         error stop 1_4
    endif

    if(any(max((/"ddd", "ddd"/), "bbb") .ne. "ddd")) error stop 2_4

    call sub1(max((/"ddd", "ddd"/), "bbb"), 2)

    if(any(func1(max((/"gko", "gko", "gko", "gko"/), (/"fgh", "fgh", "fgh","fgh"/)),2) .ne. "gk")) error stop 4_4

    contains
         subroutine sub1(carg, iarg)
            character(*), dimension(*) :: carg
            integer iarg
            character*3  carray(iarg)
            character*3, parameter::darg(2) = "zzz"

            carray = "xxx"

            if(any(max(carg(1:2), carray, darg) .ne. "zzz")) then
                   error stop 5_4
            endif
            if(any(min(carg(1:2), carray, darg) .ne. "ddd")) then
                   error stop 6_4
            endif

         end subroutine

  end program mxminLiteralArrArgSub1

  function func1(carg, n)
       character(*), dimension(*) :: carg
       character(n), dimension(2*n) :: func1
       do i = 1, 2*n
           func1(i) = carg(i)
       end do
  end function

