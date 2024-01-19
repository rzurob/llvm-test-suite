!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with literal as actual
!*                               argument in procedure pointer with
!*                               explicit interface
!*                               literal is substring
!* ===================================================================

  module literalObj
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
  end module literalObj

  program mxminLiteralScalarObj1

  use literalObj

    interface
        subroutine ifacesub1(arg1, arg2)
        use literalObj
            character*3, intent(in) :: arg2
            character*3 arg1
        end subroutine

        function ifacefun1(arg)
        use literalObj
            character*3, intent(in) :: arg
            character*3 :: ifacefun1
        end function
    end interface

    procedure(ifacesub1), pointer :: pp1
    procedure(ifacefun1), pointer :: pp2

    pp1 => sub1
    pp2 => fun1

    call pp1(max("abcddd"(4:6), "ssszdfg"(1:3)), min("ddd", "sss"))

    if(pp2(min("abcddd"(4:6), "ssszdfg"(1:3))) .ne. "ddd") then
          error stop 1_4
    endif

  end program mxminLiteralScalarObj1

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

