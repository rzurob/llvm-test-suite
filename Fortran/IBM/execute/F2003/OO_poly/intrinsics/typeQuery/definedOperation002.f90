! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: definedOperation002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 11/10/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
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

program definedOperation002
use m
    interface operator(+)
        type(Child) function bPlusB2c(a, b)
        use m
            class(Base), intent(in) :: a, b
        end function
    end interface

    interface operator(-)
        type(Base) function cMinusC2b(a, b)
        use m
            class(Child), intent(in) :: a, b
        end function
    end interface

    class(*), pointer :: u1 => null()
    type(Base) :: b1
    class(Child), allocatable :: c1
    type(Child) :: c2 = Child(1, "abc")
    b1%i = 3
    allocate(c1, SOURCE=Child(4, "aaa"))

    allocate(u1, SOURCE=(b1+c1-c2))

    if(.NOT. extends_type_of(b1, u1)) error stop 1_4
    if(.NOT. extends_type_of(c1, u1)) error stop 2_4
    if(.NOT. extends_type_of(Base(1), u1)) error stop 3_4

    if(.NOT. same_type_as(u1, b1)) error stop 4_4
    if(same_type_as(u1, c1)) error stop 5_4
    if(.NOT. same_type_as(u1, Base(1))) error stop 6_4

    select type(name=>u1)
        type is (Base)
            if(name%i /= 6) error stop 7_4
        class default
            error stop 8_4
    end select
end

type(Child) function bPlusB2c(a, b)
use m
    class(Base), intent(in) :: a, b
    bPlusB2c%i = a%i + b%i
    bPlusB2c%c = "bPlusB2c"
end function

type(Base) function cMinusC2b(a, b)
use m
    class(Child), intent(in) :: a, b
    cMinusC2b%i = a%i - b%i
end function
