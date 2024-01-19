! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/transfer/reshape003.f
! opt variations: -qnok -ql -qreuse=none

!***********************************************************************
!* =====================================================================
!* TEST BUCKET                : intrinsics/transfer
!* DATE                       : 12/21/2004
!* PRIMARY FUNCTIONS TESTED   : transfer
!* SECONDARY FUNCTIONS TESTED :
!* DESCRIPTION                :
!*   SOURCE or MOLD is function return of reshape. Poly and unlimited
!* poly.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 04/28/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Due to the cancellation of defect
!*                                 297555, this file was rewinded to
!*                                 version 1. Now use defect 297792 to
!*                                 restore the changes.
!*                              2) Also removed the TRUN header.
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program reshape003
use m
    class(AbstractParent(4)), pointer :: c1(:)
    class(*), pointer :: b1(:)

    allocate(c1(5), SOURCE=(/(Child(4)(i,i-1),i=101,105)/))
    allocate(b1(30), SOURCE=(/(Base(4)(i),i=1,30)/))

    select type(name1=>transfer(reshape(c1, (/2,4/), &
     (/Child(4)(-1,-2)/), (/2,1/)), reshape(b1, &
     (/2,3,3/))))
        type is (Base(4))
            print *, name1
            if(size(name1) .NE. 16) error stop 1_4
        class default
            error stop 2_4
    end select
end
