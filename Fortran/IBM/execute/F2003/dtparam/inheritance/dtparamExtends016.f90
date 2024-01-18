!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/29/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: test the type parameters used in the
!                               declaration of procedure pointer components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, l)
        integer, kind :: k
        integer, len :: l
    end type

    type, extends(base) :: child
        procedure(integer(k)), nopass, pointer :: p1 => null()
        procedure(character(l)), nopass, pointer :: p2 => null()
    end type
end module

program dtparamExtends016
use m
    procedure(integer(4)) i1
    procedure(character(20)) ch1

    type (child(selected_int_kind(5), 20)) c1

    c1%p1 => i1
    c1%p2 => ch1

    print *, c1%p1(100)
    print *, c1%p2()
end


integer(4) function i1(i2)
    integer i2

    integer(4), parameter :: minValue = 10

    i1 = max (minValue, i2)
end function

character(*) function ch1()
    ch1 = 'xlftest; test xlf compiler'
end function
