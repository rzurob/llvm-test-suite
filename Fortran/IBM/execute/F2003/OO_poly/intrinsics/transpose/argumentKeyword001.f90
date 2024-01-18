!***********************************************************************
!* =====================================================================
!* DATE                       : 12/30/2004
!* PRIMARY FUNCTIONS TESTED   : transpose
!* DESCRIPTION                :
!*   Actual arguments are specified using argument keywords.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 03/29/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Removed TRUN header.
!*                              2) Change test case: poly entity cannot
!*                                 be processed by regular IO.
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program argumentKeyword001
use m
    class(Base), pointer :: b2(:,:)

    allocate(b2(2,4), SOURCE=reshape((/(Base(i),i=1,8)/),(/2,4/)))

    print *, transpose(MATRIX=reshape((/(Base(i),i=1,8)/),(/2,4/)))

    select type(name1=>transpose(MATRIX=b2))
        type is (Base)
            print *, name1
        class default
            error stop 1_4
    end select
end
