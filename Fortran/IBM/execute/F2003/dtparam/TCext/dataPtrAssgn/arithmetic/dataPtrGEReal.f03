! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrGEReal.f
! opt variations: -qnol -qnodeferredlp

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
!* - lb/ub of data-ptr are statement functions
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)      :: tar(20)
    end type

    type(base(:,4)), target, allocatable :: b1
    real, pointer :: ptr(:,:)

    integer get_lb, get_ub, dim
    ! statement function
    get_lb(dim) = dim*2
    get_ub(dim) = dim**3 + 2

    allocate(base(20,4) :: b1)
    b1%tar = (/(real(i,4), i= -20,-1) /)

    call sub(b1%tar(7:20))

    if ( .not. associated(ptr)) error stop 1
    if ( .not. all(lbound(ptr) .eq. (/2,4/))) error stop 2
    if ( .not. all(ubound(ptr) .eq. (/3,10/))) error stop 3

    if ( .not. all(ptr >= reshape((/(1.0*i, i=-14,-1)/), (/2,7/) ))) error stop 5

    contains
        subroutine sub(arg)
            real, target :: arg(:)

            ptr(get_lb(1):get_ub(1), get_lb(2):get_ub(2)) => arg
        end subroutine

end program