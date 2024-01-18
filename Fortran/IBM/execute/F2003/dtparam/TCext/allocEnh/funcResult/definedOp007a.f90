! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/definedOp007a.f
! opt variations: -qnock -qnok -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/26/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               An similar case to definedOp007, except the
!                               component is of array.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        character(:), allocatable :: str(:)

        contains

        procedure :: concat => concatBaseStr
        generic :: operator(//) => concat
    end type

    contains

    character(:) function concatBaseStr (b, c)
        class(base(4,*)), intent(in) :: b
        character(*), intent(in) :: c

        allocatable concatBaseStr(:)

        if (allocated(b%str)) then
            concatBaseStr = b%str // c
        else
            concatBaseStr = [c]
        end if
    end function
end module

program definedOp007a
use m
    type(base(4,:)), allocatable :: b1(:)

    b1 = [base(4,20)(null()), base(4,20)(null())]

    b1(1) = base(4,20)(b1(1) // 'xlftest')

    b1(2) = base(4,20)([b1(1)%str, 'tsetflx'])

    b1(2) = base(4,20)(b1(2)%str // ' 101')

    print *, b1(1)%str, '|', b1(2)%str
end
