!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with variable as actual
!*                               argument to subprogram - with optional
!*                               argument using function return result,
!*                               max/min itself or named constant.
!*
!* ===================================================================

  module subVarArg2
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
  end module subVarArg2

  program mxminVarArrArgSub2

    use subVarArg2

    interface
        function func1(carg, n)
          character(*), dimension(*) :: carg
          character(n), dimension(2*n) :: func1
        end function
    end interface

    character*3 x(6), z1(2,3) , z2(2,3)
    character*3 y

    x = "ddd"
    y = "bbb"
    z1="gko"
    z2="fgh"

    ! max/min as its own argument

    call sub2(max(z1, z2, min(z1, z2, z1), max(z1, "aaa"), z1))

    if(any(LLT(min(x, y), max(x, y)) .neqv. .true.))then
         error stop 1_4
    endif

    if(any(max(x, y) .ne. "ddd")) then
          error stop 2_4
    endif

    ! function return as argument

    call sub1(max(x,y, func1(max(z1, z2), 3)), 6)

    if(any(LGT(func1(max(z1, z2),3), func1(min(z1,z2),3))                &
       .neqv. .true.)) then
           error stop 3_4
    endif

    if(any(func1(max(z1, z2),3) .ne. "gko")) then
           error stop 4_4
    endif

    ! intrinsic as its argument

    call sub2(max(z1, z2, reshape(x,(/2,3/)), z2, z1, z2, z1, z2))

    contains
         subroutine sub1(carg, iarg)
            character(*), dimension(*) :: carg
            integer iarg
            character*3  carray(iarg)
            character*3, parameter :: darg(6) = "zzz"

            carray = "xxx"

            if(any(max(carg(1:6), carray, darg) .ne. "zzz")) then
                   error stop 5_4
            endif
            if(any(min(carg(1:6), carray, darg) .ne. "gko")) then
                   error stop 6_4
            endif

         end subroutine

  end program mxminVarArrArgSub2

  function func1(carg, n)
       character(*), dimension(*) :: carg
       character(n), dimension(2*n) :: func1
       do i = 1, 2*n
           func1(i) = carg(i)
       end do
  end function
