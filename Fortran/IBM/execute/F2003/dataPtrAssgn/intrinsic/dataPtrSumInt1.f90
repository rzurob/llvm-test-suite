!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSumInt1.f 
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
!*  DESCRIPTION   use data-pointer as args of intrinsic SUM;
!*		  data-pointer of type class(*), a component of DT
!*		  data-target is array-section
!*	  	  data_pointer with bounds_remapping-list ;
!*  
!234567890123456789012345678901234567890123456789012345678901234567890

module m
	type A 
	    class(*), pointer :: ptr(:,:,:,:)
	end type
	integer, pointer :: ub

end module

program main
	use m
        integer*2, target, allocatable :: t_int(:)
	class(A), allocatable :: a1 

	allocate(a1)
        allocate(ub, source=func(2)) 

        allocate(t_int(16*16), source= (/ (int(i,2), i=0,255) /) )

	! data-pointer of 4-rank
	a1%ptr(func(-1):1,0:ub,lbound(t_int,1):2,1:func(3)) => t_int(41:)

	if ( .not. associated(a1%ptr) ) stop 5
        if ( any (lbound(a1%ptr) .ne. (/0,0,1,1 /))) stop 7	
	if ( any (ubound(a1%ptr) .ne. (/1,3,2,4 /))) stop 9 

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
