! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_type/struct_constr/fconstr015a1.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (generic name used in
!                               places of structure constructor for private
!                               component; also user defined assignment)
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
    !! private component data prevent this type to have structure constructor
    !out of the module
    type base(n1,k1)    ! (20,4)
        integer, kind                 :: k1
        integer, len                  :: n1
        integer(k1), pointer, private :: data (:) => null()

        contains

        procedure :: print => printBase
        final :: freeData
    end type

    interface base
        module procedure createBase
    end interface

    interface assignment(=)
        module procedure base2Base
    end interface

    contains

    subroutine base2Base (b1, b2)
        type (base(*,4)), intent(out) :: b1
        type (base(*,4)), intent(in) :: b2

        if (associated (b2%data)) then
            allocate (b1%data (lbound(b2%data,1):ubound(b2%data,1)), &
                source=b2%data)
        end if
    end subroutine

    type (base(20,4)) function createBase (data)
        integer*4, intent(in) :: data(2:)

        allocate (createBase%data(size(data)), source=data)

        if (lbound (createBase%data, 1) /= 1) error stop 10_4
    end function


    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        if (associated (b%data)) then
            write (*, *) b%data
        end if
    end subroutine

    subroutine freeData (b)
        type (base(*,4)), intent(inout) :: b

        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
        end if
    end subroutine
end module


program fconstr015a1
use m
    integer*4 :: i1 (3)
    type (base(20,4)) :: b1

    integer*4, allocatable :: i2(:)

    i1 = (/3,2,1/)

    b1 = base(i1)

    call b1%print

    allocate (i2(-1:1), source=(/-1,0,1/))

    b1 = base(i2)

    call b1%print
end