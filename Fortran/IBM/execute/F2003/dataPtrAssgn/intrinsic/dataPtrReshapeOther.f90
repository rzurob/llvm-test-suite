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
!* - date-targets are assumed-size arrays of diff ranks
!* - type logical*1 & integer
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

	integer, pointer :: ip(:,:)
	logical*1, pointer :: lp(:,:)

	interface foo
	    subroutine intsub(a)
		integer, target :: a(*)
	    end subroutine

	    subroutine logsub(a)
	        logical*1 :: a(:,:)
	    end subroutine
	end interface

end module

    program main

        use m

        integer, target, allocatable :: itar(:,:)
        logical(1), target :: ltar(5,1)

	allocate(itar(5,5), source=reshape((/(i,i=1,25)/),(/5,5/)))

	ltar = reshape( logical((/ kind(ltar(1,1))>1,kind(ltar(1,1))==1, &
             kind(ltar(1,1)) <1, .true., .false. /), 1), (/5, 1/))

	call intsub(itar)

	if ( .not. associated(ip)) error stop 3
	if ( any(lbound(ip) .ne. (/3,4/))) error stop 5
	if ( any(ubound(ip) .ne. (/5,6/))) error stop 7

	print *, ip
	ip =  reshape((/ip(3,:),ip(4,:),ip(5,:) /), (/3,3/) )
	print *, ip

	call logsub(ltar)

	if ( .not. associated(lp)) error stop 13
	if ( any(lbound(lp) .ne. (/-1,3/))) error stop 15
	if ( any(ubound(lp) .ne. (/3,3/))) error stop 17

	print *, lp
	print *, reshape(ltar(:,1), (/5/))

    end program

	    subroutine intsub(a)
		use m, only : ip
		integer, target :: a(*)

		ip(3:5,4:6) => a(2:20:2)
	    end subroutine

	    subroutine logsub(a)
		use m, only : lp
	        logical*1, target :: a(-1:3, 3:*)

	        lp(lbound(a,1):, lbound(a,2):) => a(:,:3)
	    end subroutine
