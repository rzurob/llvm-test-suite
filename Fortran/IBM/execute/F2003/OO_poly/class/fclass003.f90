! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (final binding with defined
!                               assignment)
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
        integer*4, pointer :: data(:) => null()

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

    contains

    subroutine finalizeBase(b)
        type (base), intent(inout) :: b

        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
        else
            print *, 'data is not associated'
        end if
    end subroutine
end module

program fclass003
use m
    interface assignment (=)
        subroutine baseAssgn2Base (a, b)
        use m
            type (base), intent(out) :: a
            type (base), intent(in) :: b

        end subroutine
    end interface

    type (base) b1, b2, b3
    type (child) c1

    allocate (b1%data(10))

    b1%data = (/(i, i=1, 10)/)

    b2 = b1

    if (associated (b2%data, b1%data)) error stop 1_4

    print *, b2%data

    b2 = b3

    print *, 'after'

    if (associated (b2%data))  error stop 2_4

    c1%base = b1

    if (associated (c1%data, b1%data)) error stop 3_4

    print *, c1%data

    c1%base = b2

    if (associated (c1%data)) error stop 4_4
end

subroutine baseAssgn2Base (a, b)
use m
    type (base), intent(out) :: a
    type (base), intent(in) :: b

    if (associated (a%data)) error stop 1_4

    if (associated (b%data)) then
        allocate (a%data(size(b%data)))

        a%data = b%data
    end if
end subroutine
