!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with variable as actual
!*                               argument in procedure pointer with explicit
!*                               interface.
!* ===================================================================

  module proc_pointer_var
    interface
        subroutine sub1(arg1, arg2)
            integer, intent(in) :: arg2(2)
            integer arg1(2)
        end subroutine

        function fun1(arg)
            integer, intent(in) :: arg
            integer :: fun1
        end function
    end interface
  end module proc_pointer_var

  program mxminlocVarArgObj3

    use proc_pointer_var

    interface
        subroutine ifacesub1(arg1, arg2)
        use proc_pointer_var
            integer, intent(in) :: arg2(2)
            integer arg1(2)
        end subroutine

        function ifacefun1(arg)
        use proc_pointer_var
            integer, intent(in) :: arg
            integer :: ifacefun1
        end function
    end interface

    character*3 x(2,3), y(10)
    integer v(2)

    procedure(ifacesub1), pointer :: pp1
    procedure(ifacefun1), pointer :: pp2

    x = reshape((/"bbb", "aaa", "ccc", "ddd","fdf", "ggg"/),(/2,3/))
    y = (/(char(i+70), i = 1,10,1)/)

    pp1 => sub1
    pp2 => fun1

    v = 2

    call pp1(v, minloc(x, dim=2))

    if(v(1) .ne. 1 .or. v(2) .ne. 1) error stop 1_4

    if(pp2(maxloc(y, dim=1, mask=.true.)) .ne. 10) error stop 2_4

  end program mxminlocVarArgObj3

  subroutine sub1(arg1, arg2)
    integer arg1(2)
    integer, intent(in) :: arg2(2)
    arg1 = arg2
  end subroutine

  function fun1(arg)
    integer, intent(in) :: arg
    integer :: fun1
    fun1 = arg
  end function
