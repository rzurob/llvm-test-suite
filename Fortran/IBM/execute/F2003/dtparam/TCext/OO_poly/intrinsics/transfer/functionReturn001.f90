! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/transfer/functionReturn001.f
! opt variations: -ql -qdefaultpv -qreuse=none

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY 
!* =====================================================================
!* TEST CASE TITLE            : intrinsics/transfer/functionReturn001.f
!* PROGRAMMER                 : Yong Du
!* DATE                       : 12/30/2004
!* ORIGIN                     :
!* PRIMARY FUNCTIONS TESTED   : transfer
!* SECONDARY FUNCTIONS TESTED :
!* DRIVER STANZA              : xlf90
!* DESCRIPTION                :
!*   SOURCE is the return value of an internal function call.
!* KEYWORD(S)                 :
!* TARGET(S)                  :
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 03/29/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Removed the TRUN header.
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

    type Base1(k2)    ! (4)
        integer, kind  :: k2
        integer(k2)       j
        type(Base(k2)) :: k
        integer(k2)       m
    end type
end module

program functionReturn001
use m
    class(*), pointer :: b1(:,:) => null()

    print *, transfer(func1(), Child(4)(1,1), 4)
    associate(name1=>transfer(func2(), (/Base1(4)(1,Base(4)(1),1)/)))
        if(size(name1) .NE. 6) error stop 1_4
        print *, name1
    end associate

    contains

    function func1()
        type(Base(4)) :: func1(20)
        func1 = (/ (Base(4)(i), i=1,20) /)
    end function

    function func2()
        class(Base(4)), pointer :: func2(:)
        allocate(func2(9), SOURCE=(/ (Child(4)(i,i+1), i=1,9) /))
    end function
end
