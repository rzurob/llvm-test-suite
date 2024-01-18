! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_poly/intrinsics/pack/argAssociation006.f
! opt variations: -qnol -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is a dummy argument. Dummy
!                              argument is a pointer or allocatable,
!                              unlimited poly, and is array.
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
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j
    end type
end module

program argAssociation006
use m
    class(*), pointer :: b(:)
    class(*), allocatable :: c(:,:)

    allocate(b(10), SOURCE=(/(Child(20,4,20,4)(i,i+1),i=1,10)/))
    allocate(c(2,3), SOURCE=reshape((/(Child(20,4,20,4)(i,i+2),i=1,5)/), &
     (/2,3/), (/Child(20,4,20,4)(1,-1)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(*), pointer :: arg1(:)
        class(*), allocatable :: arg2(:,:)

        select type(name1=>pack(arg2, reshape((/.FALSE.,.TRUE.,.TRUE., &
         .FALSE.,.TRUE.,.FALSE./),(/2,3/)), arg1(3:9)))
            type is (Child(*,4,*,4))
                print *, name1
                print *, shape(name1)
            class default
                error stop 1_4
        end select
    end subroutine
end
