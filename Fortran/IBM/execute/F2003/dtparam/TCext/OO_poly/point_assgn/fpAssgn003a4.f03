! GB DTP extension using:
! ftcx_dtp -qck -qnol -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn003a4.f
! opt variations: -qnock -ql -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (non-poly pointer array
!*                               assigned to poly-pointer array)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    class (base(4)), pointer :: b1_m(:)
    class (child(4,1,20)), allocatable, target :: c1_m(:)

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fpAssgn003a4
use m
    type (base(4)), pointer :: b_ptr(:)

    type (child(4,1,20)), target :: c1(10:30)

    class (child(4,1,20)), pointer :: c_ptr(:)
    type (child(4,1,20)), pointer :: c_ptr2(:)

    allocate (c_ptr(10))

    c_ptr%id = (/(i, i=1,10)/)
    c_ptr%name = 'c_ptr'

    b1_m => c_ptr
    b_ptr => b1_m      ! assign to compatible type

    if (.not. associated (b_ptr, c_ptr%base)) error stop 1_4

    do i = 1, 10
        call b_ptr(i)%print
    end do


    print *, 'test c_ptr2'

    c_ptr2 => c_ptr

    if (.not. associated (b_ptr, c_ptr2%base)) error stop 2_4

    do i = 1, 10
        call c_ptr2(i)%print
    end do

    deallocate(c_ptr)


    print *, 'test c1'

    c1 = (/(child(4,1,20) (i, 'c1'), i=10,30)/)

    c_ptr => c1
    b_ptr => c_ptr%base


    if ((size (b_ptr) /= 21) .or. (lbound(b_ptr,1) /= 1) .or. &
        (ubound(b_ptr,1) /= 21))    error stop 3_4

    do i = 1, 20, 2
        call b_ptr(i)%print
        call c_ptr(10+i)%print
    end do

    call b_ptr(21)%print

    if (.not. associated (b_ptr, c1%base)) error stop 4_4

    !! use module variables
    print *, 'test b1_m, c1_m'

    b1_m => c1
    c1 = (/(child(4,1,20)(10*i, 'b1_m'), i=10,30)/)

    b_ptr => b1_m

    if ((lbound(b_ptr,1)/= 10) .or. (ubound(b_ptr, 1) /= 30)) error stop 5_4

    do i = 10, 29, 2
        call b1_m(i)%print
        call b_ptr(i+1)%print
    end do

    allocate (c1_m(5))

    c1_m%id = (/1,2,3,4,5/)
    c1_m%name = 'c1_m'

    b1_m => c1_m

    b_ptr => b1_m(2::2)

    do i = 1, 5
        call b1_m(i)%print
    end do

    call b_ptr(1)%print

    call b_ptr(2)%print
end