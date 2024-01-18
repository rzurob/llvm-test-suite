! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transfer/associate005.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!***********************************************************************
!* =====================================================================
!* TEST BUCKET                : intrinsics/transfer
!* DATE                       : 10/25/2004
!* PRIMARY FUNCTIONS TESTED   : transfer
!* SECONDARY FUNCTIONS TESTED :
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
        type(Base(k1,n1)) :: b(5,6)
    end type
end module

program associate005
use m
    class(AbstractParent(4,:)), allocatable :: ap1(:,:,:)
    class(Child(4,:)), pointer :: m1 => null()
    allocate(ap1(2,3,2), SOURCE=reshape((/(Base(4,20)(i), i=1,8)/), &
     (/2,3,2/), (/Base(4,20)(-1),Base(4,20)(-2)/), (/1,2,3/)))
    allocate(m1, SOURCE=Child(4,20)(8,9,reshape((/(Base(4,20)(i),i=1,30)/), &
     (/5,6/))))

    associate(name1=>m1%b(1:5:2,2:6:2), name2=>ap1)
        select type(name3=>transfer(name1, name2))
            type is (Base(4,*))
                print *, name3
            class default
                error stop 1_4
        end select
    end associate

    associate(name1=>m1, name2=>ap1)
        select type(name3=>transfer(name1, name2))
            type is (Base(4,*))
                print *, name3
            class default
                error stop 2_4
        end select
    end associate
end
