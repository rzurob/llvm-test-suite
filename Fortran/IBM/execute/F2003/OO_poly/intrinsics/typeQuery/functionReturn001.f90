! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/02/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : A or MOLD is the return value of a
!*                               function call (internal or external).
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type

    contains

    function func4()
        class(*), pointer :: func4
        allocate(integer::func4)
    end function

    function func5()
        class(*), allocatable :: func5
        allocate(Base::func5)
    end function

    function func6()
        class(*), pointer :: func6
        allocate(Child::func6)
    end function
end module

program functionReturn001
use m
    type(Base) :: arg1
    class(*), pointer :: arg2 => null()

    if(.NOT. extends_type_of(arg1, func1())) error stop 1_4
    if(.NOT. extends_type_of(arg1, func2())) error stop 2_4
    if(.NOT. extends_type_of(func3(), arg1)) error stop 3_4
    if(extends_type_of(arg1, func4())) error stop 4_4
    if(.NOT. extends_type_of(arg1, func5())) error stop 5_4
    if(.NOT. extends_type_of(func6(), arg1)) error stop 6_4

    if(.NOT. same_type_as(arg1, func1())) error stop 7_4
    if(.NOT. same_type_as(arg1, func2())) error stop 8_4
    if(same_type_as(func3(), arg1)) error stop 9_4
    if(same_type_as(arg1, func4())) error stop 10_4
    if(.NOT. same_type_as(arg1, func5())) error stop 11_4
    if(same_type_as(func6(), arg1)) error stop 12_4

    allocate(Child::arg2)

    if(.NOT. extends_type_of(arg2, func1())) error stop 13_4
    if(.NOT. extends_type_of(func1(), func2())) error stop 14_4
    if(.NOT. extends_type_of(func3(), arg2)) error stop 15_4
    if(extends_type_of(arg2, func4())) error stop 16_4
    if(.NOT. extends_type_of(func6(), func5())) error stop 17_4

    if(same_type_as(arg2, func1())) error stop 18_4
    if(.NOT. same_type_as(func1(), func2())) error stop 19_4
    if(.NOT. same_type_as(func3(), arg2)) error stop 20_4
    if(same_type_as(arg2, func4())) error stop 21_4
    if(same_type_as(func6(), func5())) error stop 22_4

    contains

    function func1()
        type(Base) :: func1
        func1%i = 10
        !func1 = Base(10)
    end function

    function func2()
        class(Base), pointer :: func2
        allocate(func2)
    end function

    function func3()
        class(Base), allocatable :: func3
        allocate(Child::func3)
    end function
end
