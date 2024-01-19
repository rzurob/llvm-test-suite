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
!* - data-target is defined by data statement
!* - data-pointer is of type class(*), zero-size pointer array, check lbound/ubound
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  program main

      class(*), pointer, dimension(:,:,:) :: l_p
      logical*8, target :: l_t(3,2,2)

      data l_t / .true., .false., .false., .true., .false., .true., &
	         .false., .false., .true., .true., .false., .false. /

      l_p(lbound(l_t,1):, ubound(l_t,2):, dim(2,3):) => l_t(:2,:2,:2)

      if ( .not. associated(l_p)) error stop 2
      if ( any(lbound(l_p) .ne. (/1,2,0/))) error stop 5
      if ( any(ubound(l_p) .ne. (/2,3,1/))) error stop 8

      select type (l_p)
	   type is (logical*8)
      		print *, l_p
		print *, count(l_p, 1)
		print *, count(l_p, 2)
		print *, count(l_p, 3)
	   class default
		stop 10
      end select

      ! l_p is zero-size array pointer
      l_p(2:0,2:1,2:-1) => l_p(:,2,1)

      if ( .not. associated(l_p)) error stop 12
      if ( any(lbound(l_p) .ne. (/1,1,1/))) error stop 15
      if ( any(ubound(l_p) .ne. (/0,0,0/))) error stop 18

      select type (l_p)
	   type is (logical*8)
		! if mask is zero-size array, count returns zero
		if  ( count(l_p) /= 0 ) error stop 20
	   class default
		stop 22
      end select

    end program
