!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrCollapse.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-target is the components of an array of derived-type; the first dim
!* of the array is collapsed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base
        byte, pointer :: ptr(:)
    end type
end module

program main
    use m

    type(base), pointer :: p(:,:)
    type(base), target :: tar(2,2)
    integer :: k = 0
    integer i, j, ii

    do i = 1, 2
        do j = 1,2
            allocate(tar(i,j)%ptr(10), source = (/(int(k*10+ii,1), ii=1,10)/))
            k = k + 1
        enddo
    enddo

    p(2:,3:) => tar

    if ( .not. associated(p, tar)) stop 1
    if ( any(lbound(p) .ne. (/ 2,3/))) stop 2
    if ( any(ubound(p) .ne. (/ 3,4/))) stop 3
    call sub(tar)

    do i = 2,3
        do j = 3,4
            if ( .not. associated(p(i,j)%ptr, tar(i-1,j-2)%ptr)) stop 6
            if ( any(lbound(p(i,j)%ptr) .ne. (/ i+j-3/))) stop 7
            if ( any(ubound(p(i,j)%ptr) .ne. (/ i+j+6/))) stop 8
            print *, p(i,j)%ptr
        enddo
    enddo

    contains

        subroutine sub(arg)
            type(base), target :: arg(2,2)
            !IBM* collapse(arg(1))

            do i = 1,2
                do j = 1,2
                    p(i+1,j+2)%ptr(i+j:) => arg(i,j)%ptr
                enddo
            enddo

        end subroutine

end program

