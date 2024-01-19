! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc510d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2005
!*
!*  DESCRIPTION                : allocate (allocation of function return is
!                               illegal)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: x => producePtr
    end type

    contains

    integer function producePtr (b)
        class (base(4)) :: b

        pointer producePtr

        nullify (producePtr)
    end function
end module

program falloc510d
use m
    type (base(4)) b1

    allocate (b1%x())           !<-- illegal
    allocate (producePtr (b1))  !<-- illegal
end
