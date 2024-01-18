! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/null/associate003.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/01/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Return of null used in the selector of
!                              associate construct. Unlimited poly and
!                              array pointer.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(n1)    ! (20)
        integer, len :: n1
    end type

    type, extends(AbstractParent) :: Base(k1)    ! (20,4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (20,4,4)
        integer, kind :: k2
        integer(k2)      j
        class(*), pointer :: b(:,:)
    end type
end module

program associate003
use m
    associate(name1=>Child(20,4,4)(1,2,null()))
        if(associated(name1%b)) error stop 1_4
    end associate
end
