! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*
!*  DESCRIPTION                : 1. FROM is of type poly
!*				 2. TO is of unlimited polymorphic,
!*                               3. TO is finalized, From is not finalized
!*				 4. 6-rank, finalizable component with 2-rank
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    integer :: numA = 0, numB = 0, numC = 0

    type A
        integer :: id
        contains
            final :: final1
            final :: final3
    end type

    type ::  B
        type(A) :: a1(2,3)
        contains
            final :: final2
    end type

    contains
        subroutine final1(arg)
            type(A), intent(inout) :: arg(:,:)
            numA = numA + 1
        end subroutine
        subroutine final3(arg)
            type(A), intent(inout) :: arg(:,:,:,:,:,:)
            numC = numc + 1
        end subroutine
        subroutine final2(arg)
            type(B), intent(inout) :: arg
            numB = numB + 1
        end subroutine
end module


program main
use m

    class(*), allocatable :: aB(:,:,:,:,:,:)
    class(A), allocatable :: aA(:,:,:,:,:,:)

    allocate( B :: aB(2,2,2,2,2,2) )

    allocate( aA(1,1,1,1,1,1), source= reshape((/(A(i), i=-100,-100)/),(/1,1,1,1,1,1/)) )

     numC = 0

    call move_alloc(aA, aB)

    if ( .not. allocated(aB) ) error stop 11
    if ( allocated(aA)) error stop 13
    if ( numA /= 64 ) error stop 21
    if ( numB /= 0 ) error stop 23
    if ( numC /= 0 ) error stop 25

    select type (aB)
        type is (A)
           print *, shape(aB)
           print *, aB(1,1,1,1,1,1)%id
    end select

end program