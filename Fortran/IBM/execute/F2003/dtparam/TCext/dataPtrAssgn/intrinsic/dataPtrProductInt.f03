! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrProductInt.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-pointer is component of DT, rank 2
!* - lbounds of data-pointer are function call/entry call to
!*          type-bound proc w pass attr
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(k1)    ! (4)
    integer, kind        :: k1
    integer(k1), pointer :: p(:,:)
    contains
        procedure, pass :: ent
        procedure, pass :: get_val
    end type

    contains
    function get_val(a)
        class(base(4)), intent(in) :: a
        integer :: res
        integer :: get_val
        get_val = dot_product(lbound(a%p), ubound(a%p) )
        return

        entry ent(a) result (res)
        res = product( ubound(a%p) )
    end function
end module

    program main
    use m

    integer :: lb1, lb2
    type(base(4)) :: b1

    allocate(b1%p(4,5), source = reshape( (/ (i,i=1,20) /), &
        (/4,5/) ))

    lb1 = b1%get_val()
    lb2 = b1%ent()

    b1%p(lb1:,lb2:) => b1%p(::2,::2)

    if ( .not. associated(b1%p)) error stop 12
    if ( any (lbound(b1%p) .ne. (/ 9,20/))) error stop 22
    if ( any (ubound(b1%p) .ne. (/ 10,22/))) error stop 32

    print *, b1%p
    print *, product(b1%p,1), product(b1%p,2), product(b1%p)

    end program