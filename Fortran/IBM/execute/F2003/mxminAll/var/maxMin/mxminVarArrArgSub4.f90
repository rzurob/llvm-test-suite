!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX/MIN with variable as actual 
!*                               argument in procedure pointer with
!*                               explicit interface  
!* ===================================================================

  module m
    interface
        subroutine sub1(arg1, arg2)
            character*3, intent(in) :: arg2
            character*3 arg1
        end subroutine

        function fun1(arg)
            character*3, intent(in) :: arg
            character*3 :: fun1
        end function
    end interface
  end module

  program mxminVarArrArgSub4 
    use m
    interface
        subroutine ifacesub1(arg1, arg2)
        use m
            character*3, intent(in) :: arg2
            character*3 arg1
        end subroutine

        function ifacefun1(arg)
        use m
            character*3, intent(in) :: arg
            character*3 :: ifacefun1
        end function
    end interface

    character*3 x, y

    procedure(ifacesub1), pointer :: pp1
    procedure(ifacefun1), pointer :: pp2

    x="ddd"
    y="sss"

    pp1 => sub1
    pp2 => fun1

    call pp1(max(x, y,"a"), min(x, y,"zz"))

    if(pp2(min(x, y)) .ne. "ddd") then
          error stop 1_4
    endif

  end program mxminVarArrArgSub4 

  subroutine sub1(arg1, arg2)
    character*3 arg1
    character*3, intent(in) :: arg2
    if(max(arg1, arg2) .ne. "sss") then
        error stop 2_4
    endif
  end subroutine

  function fun1(arg)
    character*3, intent(in) :: arg
    character*3 :: fun1
    fun1 = arg
  end function


