!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!* TEST BUCKET                : intrinsics/transfer
!* PROGRAMMER                 : Yong Du
!* DATE                       : 12/21/2004
!* PRIMARY FUNCTIONS TESTED   : transfer
!* SECONDARY FUNCTIONS TESTED :
!* DRIVER STANZA              : xlf90
!* DESCRIPTION                :
!*   SOURCE or MOLD is a structure component, which is a scalar.
!* The object containing the component is an array and is poly.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 04/28/2005
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Due to the cancellation of defect
!*                                 297558, this file was rewinded to
!*                                 version 1. Now use defect 297792 to
!*                                 restore the changes.
!*                              2) Removed TRUN header.
!*                   MM/DD/YY : 05/27/2005
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) If a data entity b is of a type which
!*                                 contains a pionter component, then
!*                                 the physical representation of b
!*                                 includes the pointer itself instead
!*                                 of the target of the pointer.
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
    type(Child) :: c2

    c1%i = reshape((/(i,i=101,120)/), (/4,5/))
    do i=1,4
        do j=1,5
            allocate(c1(i,j)%b2, SOURCE=Base(i+j))
        end do
    end do

    c2%i = -1
    allocate(c2%b2, SOURCE=Base(-2))

    select type(name1=>transfer(c1, c2%b2, 10))
        type is (Base)
            print *, size(name1)
            print *, name1(1)
        class default
            error stop 1_4
    end select
end
