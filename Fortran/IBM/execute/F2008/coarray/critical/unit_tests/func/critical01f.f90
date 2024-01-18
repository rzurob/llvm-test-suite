!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical01f.f
!*
!*  PROGRAMMER                 : David Nichols
!*  DATE                       : Oct 25, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CRITICAL Construct
!*
!*  DRIVER STANZA              : xlf2008
!*
!*  DESCRIPTION                : Testing proper functionality of
!*                               the F2008 CRITICAL Construct
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program critical01f

	integer, parameter :: x = 12
	integer, save :: a(100)[*]

	do i = 1, 1000
		critical
			if (this_image() == 1) then
				a(:) = x
			else
				if ( a(1) /= a(100) ) then
					print *, a(1), a(100)
					error stop 15
				end if
			end if
		end critical
	end do

end
