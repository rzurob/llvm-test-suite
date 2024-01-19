!***********************************************************************
!* =====================================================================
!* DATE                       : 12/31/2004
!* PRIMARY FUNCTIONS TESTED   : transpose
!* DESCRIPTION                :
!*   MATRIX is an object containing a structure component, which is
!* a poly scalar.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 03/29/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Removed the TRUN header.
!*                              2) Modified the test case: a typo (change
!*                                 transfer to transpose; print statement
!*                                 regarding pointer component.
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        class(Base), pointer :: b2
    end type
end module

program structureComponent005
use m
    type(Child) :: c1(4,5)

    c1%i = reshape((/(i,i=101,120)/), (/4,5/))
    do i=1,4
        do j=1,5
            allocate(c1(i,j)%b2, SOURCE=Base(i+j))
        end do
    end do

    associate(name1=>transpose(c1))
        if(.NOT. same_type_as(name1, c1)) error stop 1_4
        print *, name1%i

        do i=1,5
            do j=1,4
                select type(name2=>name1(i,j)%b2)
                    type is (Base)
                        print *, name2
                    class default
                        error stop 2_4
                end select
            end do
        end do
    end associate
end
