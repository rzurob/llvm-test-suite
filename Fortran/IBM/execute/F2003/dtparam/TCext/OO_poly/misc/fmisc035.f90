! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/misc/fmisc035.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/13/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 284854)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id
    end type

    contains

    elemental logical function baseEqual (b1, b2)
        type (base(*,4)), intent (in) :: b1, b2

        baseEqual = (b1%id == b2%id)
    end function
end module

program fmisc035
use m
    type (base(20,4)) :: b1(10)

    b1%id = (/(i,i=1,10)/)

!    forall (i=1:10, baseEqual (b1(i), base(20,4)(i)))
    do i = 1, 10
        if (baseEqual (b1(i), base(20,4)(i))) b1(i) = base(20,4)(-i)
    end do
!    end forall

    print *, b1
end

