! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/misc/genericMisc006.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : misc.
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : 
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i

        contains

        procedure, private :: updateBase
        generic :: update => updateBase
    end type

    contains

    elemental subroutine updateBase(b, i)
        class(base(*,4)), intent(inout) :: b
        integer, intent(in) :: i

        b%i = i
    end subroutine
end module

module m1
use m
    type, extends(base) :: child(n2,k2)    ! (20,4,20,8)
        integer, kind :: k2
        integer, len  :: n2
        real(k2)      :: r1(27)
        procedure(real(8)), nopass, pointer :: calc => null()

        contains

        generic :: update => updaterR
        procedure, private :: updaterR
    end type

    contains

    subroutine updaterR (c, r)
        class(child(*,4,*,8)), intent(inout) :: c
        real(8), intent(in) :: r(27)

        if (associated(c%calc)) then
            do i = 1, 27
                c%r1(i) = c%calc(r(i))
            end do
        else
            c%r1 = r
        end if
    end subroutine
end module

program genericMisc006
use m1
    class(base(:,4)), allocatable :: b1(:)

    real(8) d1(10, 27)

    logical(4), external :: precision_r8

    intrinsic dsqrt

    allocate (b1(10), source=child(20,4,20,8)(0, 0, dsqrt))

    d1 = reshape ((/((/(i*100+j, i=1,10)/), j=1, 27)/), (/10, 27/))
!    print *, d1

    call b1%update((/(i, i=1, 10)/))

    select type (b1)
        type is (child(*,4,*,8))
            do i = 1, 10
                call b1(i)%update(d1(i,:))

                call b1(i)%update(b1(i)%i*2)
            end do

            do i = 1, 10
                do j = 1, 27
                    if (.not. precision_r8 (b1(i)%r1(j), &
                        sqrt(1.0d0*(100*i+j)))) error stop 4_4
                end do
            end do

        class default
            error stop 3_4
    end select


    call b1%update(b1%i-2)

    if (any(b1%i /= (/(2*i-2, i=1,10)/))) error stop 5_4
end
