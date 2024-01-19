! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (temporaries created by function
!*                               calls shall be finalized; test in the forall
!*                               header in forall statement)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id

        contains

        final :: finalizeBase
    end type

    contains

    pure subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        b%id = -1
    end subroutine
end module

module m1
use m, only : base
    type (base) :: b1_m (10)
end module

program ffinal514a5
use m1
    interface operator (==)
        pure logical function baseEqual (b1, b2)
        use m
            type (base), intent(in) :: b1, b2
        end function
    end interface

    interface
        pure type (base) function produceBase (i)
        use m
            integer*4, intent(in) :: i
        end function
    end interface

    type (base), save :: b1(3)

    data b1 /base(1), base(2), base(3)/

    b1_m%id = (/(i, i=1,10)/)

    forall (i=1:10, b1_m(i) == produceBase(3))     b1_m(i)%id = -3

    forall (i=1:3, b1(i) == produceBase(i))        b1(i)%id = -i

    print *, b1_m
    print *, b1
end


pure logical function baseEqual (b1, b2)
use m
    type (base), intent(in) :: b1, b2

    baseEqual = (b1%id == b2%id)
end function


pure type (base) function produceBase (i)
use m
    integer*4, intent(in) :: i

    produceBase%id = i
end function
