! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/others/dataPtrSMP1.f
! opt variations: -qnol -qnodeferredlp

!********************************************************************
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
!* - data-ptr used in reduction statement
!* - ub of data-ptr is private var of a parallel-do region
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(n1,k1)    ! (20,4)
	integer, kind            :: k1
	integer, len             :: n1
	class(*), pointer :: p(:)
	integer(k1), allocatable :: tar(:)
    end type
end module

program main
    use m

    type(base(:,4)), target, allocatable :: b1(:)

    integer :: i, sum = 0, ub
    integer, pointer :: p(:,:)

    integer, target :: tar(10) = (/ 1,2,3,4,5,6,7,8,9,10/)

    p(1:1,1:10) => tar(10:1:-1)

    ! rank-2 pointer array used in reduction statement
    !$omp parallel do  reduction(sum)
    do j = 1, 10
        do i = 1, 1
            sum = sum + p(i,j)
        enddo
    enddo

    if ( .not. associated(p)) error stop 49
    if ( any(lbound(p) .ne. (/1, 1/))) error stop 50
    if ( any(ubound(p) .ne. (/1, 10/))) error stop 51
    if ( sum .ne. 55)  error stop 52

    !pointer assignment used in parallel do region
    allocate(base(20,4) :: b1(10))

    do i = 1, 10
        allocate(b1(i)%tar(10), source = cshift((/(i,i=1,10)/), i-1))
    enddo

    !$omp parallel do private(ub)
    do i = 1, 10
        ub = 10
	b1(i)%p(i:ub) => b1(i)%tar
    enddo

    do i = 1, 10
        if ( .not. associated(b1(i)%p)) call zzrc(i_4)
	if ( lbound(b1(i)%p, 1) /= i) call zzrc(i_4+10_4)
	if ( ubound(b1(i)%p, 1) /= 10) call zzrc(i_4+20_4)
    enddo

    do i = 1, 10
	select type(x=>b1(i)%p)
	    type is (integer)
		print *, x
 	    class default
		stop 80
 	end select
    enddo
end program

