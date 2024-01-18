! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/null/functionReturn001.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : MOLD is the return value of a function
!                              call.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j
    end type

    contains

    function func2()
        class(*), allocatable :: func2(:)
        allocate(func2(8), SOURCE=(/(Child(4,4)(i,i-1),i=11,18)/))
    end function
end module

program functionReturn001
use m
    if(associated(null(func1()))) error stop 1_4
    if(allocated(null(func2()))) error stop 2_4

    contains

    function func1()
        class(Base(4)), pointer :: func1(:,:)
        allocate(func1(3,4), SOURCE=reshape((/(Child(4,4)(i,-i),i=1,12)/), &
         (/3,4/)))
    end function
end
