!***********************************************************************
!* =====================================================================
!* TEST BUCKET                : intrinsics/transfer
!* DATE                       : 12/16/2004
!* PRIMARY FUNCTIONS TESTED   : transfer
!* SECONDARY FUNCTIONS TESTED :
!* DESCRIPTION                :
!*   SIZE is present
!*   MOLD is scalar or array
!*   SOURCE is scalar
!*   The result is a rank one array of the same type and type
!* parameters as MOLD. Its size is the specified SIZE. Its physical
!* representation matches that of SOURCE, possibly with the remainder
!* processor dependent. Poly and unlimited poly.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 04/28/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Due to the cancellation of defect
!*                                 297520, this file was rewinded. Now
!*                                 use defect 297792 to restore the
!*                                 changes. Also removed TRUN header.
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    type Base1
        integer k(2)
        type(Base) :: b
        integer j(3)
    end type

    type, extends(Base1) :: Child1
        type(Base) :: b1
        integer n(2)
    end type
end module

program transfer022
use m
    class(Base1), allocatable :: src1
    class(*), pointer :: m1
    class(*), pointer :: m2(:,:)

    allocate(src1, SOURCE=Base1((/1,2/),Base(3),(/4,5,6/)))
    allocate(Base::m1)

    select type(name1=>transfer(src1, m1, 5))
        type is (Base)
            print *, name1
        class default
            error stop 1_4
    end select

    deallocate(src1, m1)
    allocate(src1, SOURCE=Child1((/1,2/),Base(3),(/4,5,6/),Base(7), &
     (/8,9/)))
    allocate(Child::m2(2,2))

    select type(name1=>transfer(src1, m2, 12))
        type is (Child)
            print *, name1(1:4), name1(5)%i
        class default
            error stop 2_4
    end select
end
