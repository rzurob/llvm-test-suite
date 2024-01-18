! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mv_Alloc/uLimitPoly/unlimitpolyB.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/24/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               FROM/TO are dummy arg
!*                               FROM/TO has intent(inout) attribute
!*                               MOVE_ALLOC called in module procedure
!*                               FROM is not finalized. TO is. Rank 2
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

    class(*), allocatable :: o1(:,:), o2(:,:)

    integer :: numB = 0
    integer :: numA = 0

    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id
        contains
           final :: final1
    end type

    type A(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
       contains
          final :: final2
    end type

    contains

        subroutine sub( arg1, arg2 )
            class(*), intent(inout), allocatable :: arg1(:,:), arg2(:,:)
            call move_alloc(arg1, arg2)
        end subroutine

        subroutine final1(arg)
          type(base(*,4)), intent(in) :: arg(:,:)
          numB = numB + 1
       end subroutine

        subroutine final2(arg)
          type(A(4,*)), intent(in) :: arg(:,:)
          numA = numA + 1
       end subroutine

end module

program main
use m

    integer i

    allocate( o1(5,1), source = reshape((/ (base(20,4)(i), i= 11,51,10) /), (/5,1/)))
    allocate( o2(1,2), source = reshape((/ A(4,20)(), A(4,20)() /),(/1,2/)))

    numA = 0
    numB = 0

    call sub (o1, o2)

    if ( numA /= 1 ) error stop 11
    if ( numB /= 0) error stop 13

    if ( allocated(o1) ) error stop 21

    if ( .not. allocated(o2) ) error stop 23

    select type(o2)
        type is (base(*,4))
		do i = 1, 5
	           if ( o2(i,1)%id  /= 10*i + 1 )  call zzrc(i)
		enddo
        class default
            stop 25
    end select
end
