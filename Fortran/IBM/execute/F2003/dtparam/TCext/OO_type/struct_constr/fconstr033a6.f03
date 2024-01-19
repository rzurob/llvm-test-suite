! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr033a6.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/21/2005
!*
!*  DESCRIPTION                : structure constructor (named constants used as
!                               the data source for the allocatable components)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data
    end type
end module

program fconstr033a6
use m
    character(20), parameter :: comment = '!1234567890987654321'

    type (base(4,20)) b1

    b1 = base(4,20) (comment(3:5))

    if (.not. allocated (b1%data)) error stop 1_4

    select type (x => b1%data)
        type is (character(*))
            if (len(x) /= 3) error stop 2_4

            print *, x
        class default
            error stop 3_4
    end select
end
