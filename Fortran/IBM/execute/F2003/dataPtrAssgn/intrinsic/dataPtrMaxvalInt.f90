!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrMaxvalInt.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-pointer of type integer as arg of maxval
!* - pointer assignment appears in do-loop, where ub/lb vary with loop index
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    program main

	integer, parameter :: cnt = 18
	integer a(cnt)
	integer, pointer :: p(:), b(:)

	allocate(integer :: p(cnt))

	b => p

	if ( .not. associated(b)) stop 2

	do i = cnt , 1 , -2
	   p(i) = i
	   p(i-1) = i-1

	   if ( i > 2) then
		p(1:i-2) => p(i-2:1:-1)

		if ( .not. associated(p)) stop 12
		if ( lbound(p,1) /= 1 ) stop 22
		if ( ubound(p,1) /= i-2 ) stop 32
	   endif

	enddo

	print *, b
	print *, size(p), p
	print *, maxval(b), maxval(p)

    end program
