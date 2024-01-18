!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_subprogram20f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 6 December, 2012
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : F2008 submodule
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*  based on OO_procptr/component1/interfaceName002c.f
!*
!*  Accessibility of the variables of a use associated module in a
!*   submodule when the USE keyword is specified in:
!*   - the host module scope
!*   - a module subroutine definition
!*  
!*  Verify that the results match the values of the original test case.  
!*  
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module m1
    type Base
        integer i
    end type
end module


module m2
use m1

interface 


    real module function func1()
    end function

    module function func2(b)
        type(Base) :: b
        type(Base) :: func2
    end function
end interface

end module

submodule (m2) m2sub
contains


    module procedure func1
        func1 = 20
    end

end submodule

submodule (m2:m2sub) m2sub2
contains
    module procedure func2
        func2 = b
    end
endsubmodule

!---------------------------------------------------------------------!
! Test the use of m1 in use associated scope for the submodules       !
! (even though host scope promotion may be done by the compiler)      !
!---------------------------------------------------------------------!
module m3
interface
    module subroutine sub1(i, b)
        use m1
        integer, intent(in) :: i
        type(Base), intent(in) :: b
    end subroutine
end interface
end module

submodule (m3) m3sub
contains
    module procedure sub1
        print *, "sub1", i, b
    end
end submodule

module m4
    interface
        subroutine interfaceSub1(i, b)
        use m1
            integer, intent(in) :: i
            type(Base), intent(in) :: b
        end subroutine

        real function interfaceFunc1()
        end function

        function interfaceFunc2(b)
        use m1
            type(Base) :: b
            type(Base) :: interfaceFunc2
        end function
    end interface

    type Container
        procedure(interfaceSub1), nopass, pointer :: pp1
        procedure(interfaceFunc1), nopass, pointer :: pp2
        procedure(interfaceFunc2), nopass, pointer :: pp3
    end type
end module

program interfaceName002c
use m2
use m3
use m4
    type(Container) :: c1
    c1%pp1 => sub1
    c1%pp2 => func1
    c1%pp3 => func2

    call c1%pp1(10, Base(11))
    print *, "func1", int(c1%pp2())
    print *, "func2", c1%pp3(Base(5))
end
