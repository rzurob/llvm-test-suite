! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (deep copy needed for variables in
!                               source-expr with allocatable component)
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
        integer(4) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(18), allocatable :: addresses(:)

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, 'id =', b%id, '; bounds of addresses: ', lbound(b%addresses,1), &
                    ubound(b%addresses,1)

        do i = lbound(b%addresses,1), ubound(b%addresses,1)
            print *, 'address', i, ':', b%addresses(i)
        end do
    end subroutine
end module

program falloc010a
use m
    class (base), allocatable :: b1, b2

    character(18), pointer :: ch1(:)

    allocate (ch1(0:1), source=(/'8200 Warden Ave.', '3600 Steeles St.'/))

    allocate (b1, source=child(1,ch1))

    allocate (b2, source=b1)

    call b1%print

    call b2%print
end
