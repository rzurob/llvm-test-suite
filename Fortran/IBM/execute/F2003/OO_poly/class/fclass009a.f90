!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass009a.f
! %VERIFY: fclass009a.out:fclass009a.vf
! %STDIN:
! %STDOUT: fclass009a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (elemental operator (+) for
!                               derived type; test using rank-one array)
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
        integer, allocatable :: id
    end type

    interface operator(+)
        elemental type (base) function addB1B2 (b1, b2)
        import base
            type (base), intent(in) :: b1, b2
        end function
    end interface
end module

program fclass009a
use m
    type (base), allocatable :: b1(:)
    class (base), allocatable :: b2(:)

    allocate (b1(3), source=(/(base(i), i =1, 3)/))
    allocate (b2(3), source=(/(base(i), i =101, 103)/))

    associate (x => b1 + base(10))
        if (size (x) /= 3) error stop 1_4

        print *, x(1)%id, x(2)%id, x(3)%id
    end associate

    associate (x => b2 + base(10))
        if (size (x) /= 3) error stop 2_4

        print *, x(1)%id, x(2)%id, x(3)%id
    end associate
end


elemental type (base) function addB1B2 (b1, b2)
use m, only: base
    type (base), intent(in) :: b1, b2

    integer val1, val2

    val1 = 0
    val2 = 0

    if (allocated (b1%id)) val1 = b1%id

    if (allocated (b2%id)) val2 = b2%id

    allocate (addB1B2%id, source=(val1+val2))
end function
