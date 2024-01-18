!********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSMP1.f
!*
!*  PROGRAMMER                 : Michelle Zhang
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!*
!* - data-ptr used in reduction statement 
!* - ub of data-ptr is private var of a parallel-do region
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base
	class(*), pointer :: p(:)
	integer, allocatable :: tar(:)
    end type
end module

program main
    use m

    type(base), target, allocatable :: b1(:)

    integer :: i, sum = 0, ub 
    integer, pointer :: p(:,:)
   
    integer, target :: tar(10) = (/ 1,2,3,4,5,6,7,8,9,10/)

    p(1:1,1:10) => tar(10:1:-1)

    ! rank-2 pointer array used in reduction statement
    !$omp parallel do  reduction(+:sum)
    do j = 1, 10 
        do i = 1, 1 
            sum = sum + p(i,j)
        enddo
    enddo 
   
    if ( .not. associated(p)) stop 49 
    if ( any(lbound(p) .ne. (/1, 1/))) stop 50
    if ( any(ubound(p) .ne. (/1, 10/))) stop 51 
    if ( sum .ne. 55)  stop 52 

    !pointer assignment used in parallel do region
    allocate(b1(10))

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

