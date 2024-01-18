!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with derived type component
!*                               as argument. No DIM or MASK specified.
!*                               Also with automatic array.
!*  (315480)
!* ===================================================================

  module ArgSub3
     type dt_array
       character*3, dimension(5) :: b = max("zzz", "kkk")
     end type

     contains
        subroutine sub2(arg1, arg2)
           type(dt_array) :: dt_arg
           character*3 , intent(in):: arg2
           character*3 , intent(inout)::arg1
           if(arg2  .ne. "ccc") error stop 1_4
           if(arg1  .ne. "ggg") error stop 2_4
           arg1 = maxval(max(dt_arg%b, arg2))
        end subroutine

  end module ArgSub3

  program mxminvalVarArgSub1

    use ArgSub3

    interface
        function func1(carg, n)
          character*3 :: carg
          character(n) :: func1
        end function
    end interface

    type dt_init
      sequence
      character*3 :: c(4) = min("sss", "ccc", "hhh")
    end type

    character*3 :: v

    type(dt_init) :: z

    v = "ggg"

    call sub2(v, maxval(z%c))

    if(v .ne. "zzz") error stop 3_4

    call sub1(minval(z%c), 4)

    if(func1(maxval(z%c),3) .ne. "ccc") error stop 4_4

    contains
         subroutine sub1(carg, iarg)
            character(*) :: carg
            integer iarg
            character*3  carray(iarg)

            carray = "xxx"
            carray(3) = "zzz"

            if(carg .ne. "ccc") error stop 5_4
            if(maxval(carray)  .ne. "zzz") error stop 6_4

         end subroutine

  end program mxminvalVarArgSub1

  function func1(carg, n)
       character(*) carg
       character(n) func1
       func1 = carg
  end function
