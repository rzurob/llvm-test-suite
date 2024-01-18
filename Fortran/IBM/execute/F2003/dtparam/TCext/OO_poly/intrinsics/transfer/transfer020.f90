! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/transfer/transfer020.f
! opt variations: -ql -qdefaultpv -qreuse=self -qreuse=none

!***********************************************************************
!* =====================================================================
!* TEST BUCKET                : intrinsics/transfer
!* DATE                       : 12/16/2004
!* PRIMARY FUNCTIONS TESTED   : transfer
!* SECONDARY FUNCTIONS TESTED :
!* DESCRIPTION                :
!*   SIZE is absent
!*   MOLD is array
!*   SOURCE is scalar
!*   The result is a rank one array of the same type and type
!* parameters as MOLD. Its size is as small as possible and its
!* physical representation is not shorter than that of SOURCE.
!*   Poly and unlimited poly
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 04/28/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Due to the cancellation of defect
!*                                 297520, this file was rewinded to
!*                                 version 1. Now use defect 297792 to
!*                                 restore the changes.
!*                              2) Also removed TRUN header.
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    type Base1(k2,k3)    ! (4,4)
        integer, kind  :: k2,k3
        integer(k2)       k(2)
        type(Base(k2)) :: b
        integer(k3)       j(3)
    end type

    type, extends(Base1) :: Child1    ! (4,4)
        type(Base(k2)) :: b1
        integer(k2) n(2)
    end type
end module

program transfer020
use m
    class(Base1(4,4)), allocatable :: src1
    class(*), pointer :: m1(:,:)

    allocate(src1, SOURCE=Base1(4,4)((/1,2/),Base(4)(3),(/4,5,6/)))
    allocate(Base(4)::m1(2,2))

    select type(name1=>transfer(src1, m1))
        type is (Base(4))
            print *, name1(:)
        class default
            error stop 1_4
    end select

    deallocate(src1, m1)
    allocate(src1, SOURCE=Child1(4,4)((/1,2/),Base(4)(3),(/4,5,6/),Base(4)(7), &
     (/8,9/)))
    allocate(Child(4)::m1(2,2))

    select type(name1=>transfer(src1, m1))
        type is (Child(4))
            print *, name1(1:4), name1(5)%i
        class default
            error stop 2_4
    end select
end
