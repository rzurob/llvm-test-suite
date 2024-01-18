! GB DTP extension using:
! ftcx_dtp -qk -qnodeferredlp /tstdev/OO_poly/misc/fmisc036.f
! opt variations: -qck -qnok -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/13/2005
!*
!*  DESCRIPTION                : miscellaneous item (defect 289822)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        character(n1) :: name
    end type

    contains

    subroutine printBase (b)
        type (base(4,*)), intent(in) :: b

        print *, b%name
    end subroutine
end module

program fmisc036
use m
    type (base(4,20)), allocatable :: b1(:)
    type (base(4,20))  :: b2(1:2)

    allocate (b1(2))
    b1%name = 'test'

    b2%name = 'xlf'

    associate (x2 => (/b1/))
        call printBase (x2(1))
        call printBase (x2(2))
    end associate

    associate (x3 => (/b2/))
        call printBase (x3(2))
    end associate
end

