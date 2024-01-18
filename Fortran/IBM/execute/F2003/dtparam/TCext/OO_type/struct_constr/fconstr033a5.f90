! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr033a5.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/21/2005
!*
!*  DESCRIPTION                : structure constructor (named constants used as
!                               the source data for the allocatable structure
!                               components in structure constructor; use
!                               rank-one array)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data(:)
    end type
end module

program fconstr033a5
use m
    real(8), parameter :: one(2) = 1.0_8

    type (base(4,20)) :: b1

    b1 = base(4,20)(one)

    if (.not. allocated (b1%data)) error stop 1_4
    if ((lbound(b1%data,1) /= 1) .or. (ubound(b1%data,1) /= 2)) error stop 2_4

    select type (x => b1%data)
        type is (real(8))
            write (*, '(2f10.2)') x
        class default
            error stop 3_4
    end select
end
