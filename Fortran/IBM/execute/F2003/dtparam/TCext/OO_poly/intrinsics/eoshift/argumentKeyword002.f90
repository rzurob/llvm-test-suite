! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_poly/intrinsics/eoshift/argumentKeyword002.f
! opt variations: -qnol -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/03/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Actual arguments are speficied using
!                              argument keywords. Unlimited poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) :: j = 9
    end type
end module

program argumentKeyword002
use m
    class(*), pointer :: b1(:,:,:), boundVal

    allocate(b1(3,2,2), SOURCE=reshape((/(i,i=1,12)/), &
     (/3,2,2/)))

    allocate (boundVal, source=0)

    select type(name1=>eoshift(SHIFT=reshape((/1,-2,2,-1/),(/2,2/)), &
     ARRAY=b1,DIM=1, BOUNDARY=boundVal))
        type is (integer)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
