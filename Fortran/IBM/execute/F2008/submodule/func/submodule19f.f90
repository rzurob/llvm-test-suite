!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule19f
!*
!*  DATE                       : 6 December, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : F2008 submodule
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*  based on OO_procptr/component1/functionReturn003.f
!*
!*  Define a module function that sets the target of a procedure pointer
!*
!*  Secondary tests:
!*  - module function can access module functions defined in other
!*    submodules
!*  - multiple submodules of a module each defining different procedures
!*    declared in the interface:
!*                 m
!*               / | \
!*              /  |  \
!*             /   |   \
!*            m1   m2   m3
!*  - set a pointer indirectly using the module function through use
!*    association of m in module n to a pointer inside a derived type
!*  - verifies nested use association of a module with submodules
!*
!*  Verify that the results match the values of the original test case
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type Base
        integer i
    end type

    interface
        module function func1(i)
            integer, intent(in) :: i
            procedure(), pointer :: func1
        end function

        module subroutine sub2(i)
            integer, intent(in) :: i
        end subroutine

        module subroutine sub3(b, c)
            type(Base), intent(in) :: b
            type(Base), intent(in) :: c
        end subroutine
    end interface

end module

submodule (m) msub1
contains
    module function func1(i)
        integer, intent(in) :: i
        procedure(), pointer :: func1

        if(i .EQ. 1) then
            func1 => sub2
        else
            func1 => sub3
        endif
    end function

end submodule

submodule (m) msub2
contains
    module subroutine sub2(i)
        integer, intent(in) :: i
        print *, "sub2", i
    end subroutine
end submodule

submodule (m) msub3
contains
    module subroutine sub3(b, c)
        type(Base), intent(in) :: b
        type(Base), intent(in) :: c
        print *, "sub3", b, c
    end subroutine
end submodule

module n
use m
    implicit type(Base) (p)

    type Container
        procedure(), nopass, pointer :: pp1 => null()
    end type
end module

program functionReturn003
use n
    implicit type(Base) (p,f), type(Container) (c)

    if(associated(c1%pp1)) error stop 1_4

    c1%pp1 => func1(1)
    if(.NOT. associated(c1%pp1)) error stop 2_4
    call c1%pp1(5)

    nullify(c1%pp1)
    if(associated(c1%pp1)) error stop 3_4

    c1%pp1 => func1(2)
    if(.NOT. associated(c1%pp1)) error stop 4_4
    call c1%pp1(Base(6), Base(7))
end
