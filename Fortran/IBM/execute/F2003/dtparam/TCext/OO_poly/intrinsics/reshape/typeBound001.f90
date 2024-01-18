! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/reshape/typeBound001.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains
        procedure :: reshapeMe
    end type

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
    end type

    contains

    function reshapeMe(this, i, p)
        class(AbstractParent(4,*)), intent(in) :: this
        integer, intent(in) :: i
        class(AbstractParent(4,*)), OPTIONAL, intent(in) :: p(:)
        class(AbstractParent(4,:)), pointer :: reshapeMe(:,:)
        class(AbstractParent(4,:)), pointer :: src(:)
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
    type(Base(4,20,20,4)) :: b1
    type(Child(4,20,20,4,20,4)) :: c1

    b1%i = 7
    c1%i = 8
    c1%j = 9

    select type (name1=>b1%reshapeMe(25))
        type is (Base(4,*,*,4))
            print *, name1
        class default
            error stop 1_4
    end select

    select type (name2=>c1%reshapeMe(15, (/Child(4,20,20,4,20,4)(-1,1),Child(4,20,20,4,20,4)(-2,2)/)))
        type is (Child(4,*,*,4,*,4))
            print *, name2
        class default
            error stop 2_4
    end select
end
