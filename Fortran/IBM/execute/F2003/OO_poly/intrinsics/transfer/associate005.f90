!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!* TEST BUCKET                : intrinsics/transfer
!* PROGRAMMER                 : Yong Du
!* DATE                       : 10/25/2004
!* PRIMARY FUNCTIONS TESTED   : transfer
!* SECONDARY FUNCTIONS TESTED :
!* DRIVER STANZA              : xlf90
!* DESCRIPTION                :
!*   SOURCE and/or MOLD are associate names. Selector is array
!* section and is a structure component.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 04/28/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Due to the cancellation of defect
!*                                 297557, this file was rewinded to
!*                                 version 1. Now restore the changes
!*                                 using defect 297792.
!*                              2) Remove the TRUN header.
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
        type(Base) :: b(5,6)
    end type
end module

program associate005
use m
    class(AbstractParent), allocatable :: ap1(:,:,:)
    class(Child), pointer :: m1 => null()
    allocate(ap1(2,3,2), SOURCE=reshape((/(Base(i), i=1,8)/), &
     (/2,3,2/), (/Base(-1),Base(-2)/), (/1,2,3/)))
    allocate(m1, SOURCE=Child(8,9,reshape((/(Base(i),i=1,30)/), &
     (/5,6/))))

    associate(name1=>m1%b(1:5:2,2:6:2), name2=>ap1)
        select type(name3=>transfer(name1, name2))
            type is (Base)
                print *, name3
            class default
                error stop 1_4
        end select
    end associate

    associate(name1=>m1, name2=>ap1)
        select type(name3=>transfer(name1, name2))
            type is (Base)
                print *, name3
            class default
                error stop 2_4
        end select
    end associate
end
