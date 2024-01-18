! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/misc/d286493.f
! opt variations: -ql

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 286493)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind            :: k1
        integer(k1), allocatable :: data (:)
    end type

    contains

    subroutine testVal1 (b)
        type (base(4)), value :: b

        if (allocated (b%data)) then
            print *, size (b%data)
        else
            print *, 'component not allocated'
        end if
    end subroutine
end module

program fArg009a2
use m
    type (base(4)) b1

    class (base(4)), allocatable :: b2

    allocate (b1%data (5), b2)

    call testVal1 (b1)

    call testVal1 (b2)

    call testVal1 (base(4)((/(k, k=1,100,2)/)))

    call testVal1 (base(4)(null()))
end
