! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrSumInt1.f
! opt variations: -qnok -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION   use data-pointer as args of intrinsic SUM;
!*		  data-pointer of type class(*), a component of DT
!*		  data-target is array-section
!*	  	  data_pointer with bounds_remapping-list ;
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
	type A(k1,n1)    ! (4,20)
	    integer, kind :: k1
	    integer, len  :: n1
	    class(*), pointer :: ptr(:,:,:,:)
	end type
	integer, pointer :: ub

end module

program main
	use m
        integer*2, target, allocatable :: t_int(:)
	class(A(4,:)), allocatable :: a1

	allocate(A(4,20) :: a1)
        allocate(ub, source=func(2))

        allocate(t_int(16*16), source= (/ (int(i,2), i=0,255) /) )

	! data-pointer of 4-rank
	a1%ptr(func(-1):1,0:ub,lbound(t_int,1):2,1:func(3)) => t_int(41:)

	if ( .not. associated(a1%ptr) ) error stop 5
        if ( any (lbound(a1%ptr) .ne. (/0,0,1,1 /))) error stop 7
	if ( any (ubound(a1%ptr) .ne. (/1,3,2,4 /))) error stop 9

	select type (x => a1%ptr)
  	    type is (integer(2) )
		print *, x
		print *, sum(x,1)
		print *, sum(x,2)
		print *, sum(x,3)
		print *, sum(x, 4)
	    class default
		stop 11
	end select

	contains
            function func(i)
		integer i, func
		allocatable func

		allocate(func, source = i+1)
	    end function

        end program
