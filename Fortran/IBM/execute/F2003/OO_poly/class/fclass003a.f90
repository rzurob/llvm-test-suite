!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass003a.f
! %VERIFY: fclass003a.out:fclass003a.vf
! %STDIN:
! %STDOUT: fclass003a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (defined assignment and elemental
!                               final binding)
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

    elemental subroutine finalizeBase(b)
        type (base), intent(inout) :: b

        if (associated (b%data)) deallocate (b%data)
    end subroutine
end module

use m
    interface assignment (=)
        subroutine baseAssgn2Base (a, b)
        use m
            class (base), intent(out) :: a
            type (base), intent(in) :: b

        end subroutine
    end interface

    type (base) b1, b2
    type (child) c1

    allocate (b1%data(10))

    b1%data = (/(11-i, i=1, 10)/)

    b2 = b1

    if (associated (b2%data, b1%data)) error stop 2_4

    print *, b2%data

    c1 = b1

    if (associated (c1%data, b1%data)) error stop 3_4

    print *, c1%data

    c1 = b2

    if (associated (c1%data, b2%data)) error stop 3_4

    print *, c1%data

    deallocate (b1%data, b2%data, c1%data)
end

subroutine baseAssgn2Base (a, b)
use m
    class (base), intent(out) :: a
    type (base), intent(in) :: b

    if (associated (a%data)) error stop 1_4

    allocate (a%data(size(b%data)))

    a%data = b%data
end subroutine
