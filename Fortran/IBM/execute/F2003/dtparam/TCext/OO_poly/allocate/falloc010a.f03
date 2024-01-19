! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc010a.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,18)
        integer, kind                          :: k2
        integer, len                           :: n1
        character(kind=k2,len=n1), allocatable :: addresses(:)

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, 'id =', b%id, '; bounds of addresses: ', lbound(b%addresses,1), &
                    ubound(b%addresses,1)

        do i = lbound(b%addresses,1), ubound(b%addresses,1)
            print *, 'address', i, ':', b%addresses(i)
        end do
    end subroutine
end module

program falloc010a
use m
    class (base(4)), allocatable :: b1, b2

    character(18), pointer :: ch1(:)

    allocate (ch1(0:1), source=(/'8200 Warden Ave.', '3600 Steeles St.'/))

    allocate (b1, source=child(4,1,18)(1,ch1))

    allocate (b2, source=b1)

    call b1%print

    call b2%print
end
