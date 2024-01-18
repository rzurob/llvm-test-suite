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
!* - data-ptr of type class(*) & data-tar of type int are common-block-objects
!* - data-target initialized in block data stmt
!* - pointer-assignment used in main program
!* - data-ptr's association status & value verified in external proc
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    program main

	integer, target :: tar(10)
	class(*), pointer :: p(:)
	!integer, pointer :: p(:)

	common /comm1/ tar, p

!	print *, minval(tar), maxval(tar)
	p(minval(tar):maxval(tar)) => tar

	call sub

 End program

    block data block
	integer itar(10)
	class(*), pointer :: ip(:)
	!integer, pointer :: ip(:)

	common /comm1/ itar, ip
	data itar / 1,2,3,4,5,6,7,8,9,10 /
    end block data block

  subroutine sub
	integer, target ::  itar(10)
	class(*), pointer :: ip(:)
	!integer,  pointer :: ip(:)

	common /comm1/ itar, ip

	if ( .not. associated(ip, itar)) error stop 29
	if ( any(lbound(ip,1) .ne. (/1/))) error stop 31
	if ( any(ubound(ip,1) .ne. (/ 10/))) error stop 33

	select type(ip)
	    type is (integer)
		if ( any(+(-ip) .ne. (/ (-i,i=1,10)/))) error stop 35
	    class default
		stop 50
	end select
  end subroutine
