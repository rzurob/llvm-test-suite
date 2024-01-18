! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr504.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (a nested allocatable
!                               component)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), allocatable :: x
    end type

    integer, save :: counter = 0

    contains

    recursive subroutine printType (b)
        class (base(4,*)), intent(in) :: b

        if (.not. allocated (b%x)) return

        select type (x =>b%x)
            type is (base(4,*))
                counter = counter + 1
                call printType (x)
            class default
                error stop 1_4
        end select
    end subroutine
end module

program fconstr504
use m
    type (base(4,20)) :: b1
    b1 = base(4,20) (x = base(4,20)(x = base(4,20)(x = base(4,20)(null()))))

    call printType (b1)

    if (counter /= 3) error stop 5_4
end

