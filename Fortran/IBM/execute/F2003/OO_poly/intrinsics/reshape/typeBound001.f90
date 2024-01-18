!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!* TEST CASE TITLE            : OO_poly/intrinsics/reshape/typeBound001.f
!* PROGRAMMER                 : Yong Du
!* DATE                       : 11/07/2004
!* ORIGIN                     :
!* PRIMARY FUNCTIONS TESTED   : reshape
!* DRIVER STANZA              : xlf90
!* DESCRIPTION                : Call the intrinsic inquiry functions
!*   inside the type bound procedures. Cross testing OPTIONAL.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 03/24/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Removed TRUN header.
!*                              2) Updated the source file: initialize
!*                                 b1 and c1.
!*                              3) Updated the verification file.
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
        contains
        procedure :: reshapeMe
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    function reshapeMe(this, i, p)
        class(AbstractParent), intent(in) :: this
        integer, intent(in) :: i
        class(AbstractParent), OPTIONAL, intent(in) :: p(:)
        class(AbstractParent), pointer :: reshapeMe(:,:)
        class(AbstractParent), pointer :: src(:)
        allocate(src(i), SOURCE=this)
        if(present(p)) then
            allocate(reshapeMe(4,5), &
             SOURCE=reshape(src, (/4,5/), p, (/1,2/)))
        else if(i .LT. 20) then
            allocate(reshapeMe(4,5), &
             SOURCE=reshape(src, (/4,5/), src, (/2,1/)))
        else
            allocate(reshapeMe(4,5), SOURCE=reshape(src, (/4,5/)))
        end if
    end function
end module

program typeBound001
use m
    type(Base) :: b1
    type(Child) :: c1

    b1%i = 7
    c1%i = 8
    c1%j = 9

    select type (name1=>b1%reshapeMe(25))
        type is (Base)
            print *, name1
        class default
            error stop 1_4
    end select

    select type (name2=>c1%reshapeMe(15, (/Child(-1,1),Child(-2,2)/)))
        type is (Child)
            print *, name2
        class default
            error stop 2_4
    end select
end
