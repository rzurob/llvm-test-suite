!***********************************************************************
!* =====================================================================
!* DATE                       : 12/30/2004
!* PRIMARY FUNCTIONS TESTED   : transfer
!* SECONDARY FUNCTIONS TESTED :
!* DESCRIPTION                :
!*   SOURCE or MOLD is the return value of intrinsic function reshape().
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 03/29/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Removed TRUN header.
!*                              2) Changed (/3,3/) to (/5,2/).
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer :: i = 1
    end type

    type, extends(Base) :: Child
        integer :: j = 2
    end type
end module

program functionReturn003
use m
    class(*), pointer :: b1(:)
    class(Base), allocatable :: b2(:)

    allocate(b1(10), SOURCE=(/(Base(i),i=-1,-10,-1)/))
    allocate(b2(10), SOURCE=(/(Child(i,i+1),i=11,20)/))

    select type(name1=>transfer(reshape(b1,(/5,2/)), &
     reshape(b2,(/2,2/))))
        type is (Child)
            print *, name1
        class default
            error stop 1_4
    end select
end
