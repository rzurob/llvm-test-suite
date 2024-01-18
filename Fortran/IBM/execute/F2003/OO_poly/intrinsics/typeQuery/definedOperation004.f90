! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: definedOperation004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/10/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Use defined operation to change the
!*    dynamic types.
!*
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
end module

program definedOperation004
use m
    interface operator(+)
        function bPlusB2c(a, b)
        use m
            class(Base), intent(in) :: a(:), b(size(a))
            type(Child) :: bPlusB2c(size(a))
        end function
    end interface

    class(*), allocatable :: u1(:)
    type(Base) :: b1(5)
    class(Child), allocatable :: c1(:)
    b1%i = 1
    allocate(c1(size(b1)))
    c1%i = 2
    c1%c = "abc"
    allocate(u1(size(b1)), SOURCE=(b1+c1))

    if(.NOT. extends_type_of(u1, b1)) error stop 1_4
    if(.NOT. extends_type_of(u1, c1)) error stop 2_4
    if(.NOT. extends_type_of(u1, Base(1))) error stop 3_4

    if(same_type_as(u1, b1)) error stop 4_4
    if(.NOT. same_type_as(u1, c1)) error stop 5_4
    if(.NOT. same_type_as(u1, Child(1, "a"))) error stop 6_4

    select type(name=>u1)
        type is (Child)
            if(name(1)%i /= 3) error stop 7_4
            if(name(5)%c /= "bPlusB2c") error stop 8_4
        class default
            error stop 9_4
    end select
end

function bPlusB2c(a, b)
use m
    class(Base), intent(in) :: a(:), b(size(a))
    type(Child) :: bPlusB2c(size(a))
    bPlusB2c%i = a%i + b%i
    bPlusB2c%c = "bPlusB2c"
end function
