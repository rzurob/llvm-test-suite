! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/mv_Alloc/uLimitPoly/unlimitpolyF.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimited polymorphic,
!*                               rank 1
!*				 FROM is local var, TO is component of derive-type
!*                               TO is finalized
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    integer :: numA = 0
    type A(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id

        contains
            final :: finalA
    end type

    contains
        subroutine finalA(arg)
            type(A(*,4)), intent(in)  :: arg(:)
            numA = numA + 1
        end subroutine
end module


program main
use m

    integer i
    class (*), allocatable :: x(:)

    type base(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        class (*), allocatable :: value(:)
    end type

    class(base(4,:)), allocatable :: b1

    allocate(x(5), source = (/ ( A(20,4)(i), i = 1, 5 ) /) )
    allocate(b1, source = base(4,20)(x) )

    deallocate(x)

    allocate (x(2), source= (/ (1.1d2, 2.5d1), (4.8d2, 9.6d1) /) )

    numA = 0

    call move_alloc( x , b1%value )

    if ( numA /= 1 ) error stop 31
    if ( allocated(x) ) error stop 41
    if ( .not. allocated(b1%value) ) error stop  51

    select type( p => b1%value)
        type is (complex(8))
            write (*, '(2d12.3)')  p
        class default
           stop 33
   end select
   end

