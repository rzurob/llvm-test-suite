! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/point_assgn/fpAssgn003a7.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (non-poly data pointer
!*                               assigned to poly, or ancestor component of the
!*                               data-target; test its dynamic types using
!*                               type bound in procedure calls)
!*
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
        integer(k1)   :: id = 0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: name = ''

        contains

        procedure :: print => printChild
    end type

    type (base(:,4)), pointer :: b(:)

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printData (b)
        class (base(*,4)), intent(in) :: b(:)

        do i = 1, size(b)
            call b(i)%print
        end do
    end subroutine
end module

program fpAssgn003a6
use m, only : base, child, printData, b

    class (base(:,4)), pointer :: b_ptr(:)
    class (child(20,4,4,20)), pointer :: c1 (:)

    allocate (c1(10))

    c1%id = (/(i, i=1,10)/)
    c1%name = 'c1'

    b => c1(::3)%base

    if (size(b) /= 4) error stop 1_4

    call printData (b)
    call printData (c1(::3))

    b_ptr => c1(::4)

    call printData (b_ptr)

    b => b_ptr

    if (size(b) /= 3) error stop 2_4

    call printData (b)

    deallocate (c1)
end
