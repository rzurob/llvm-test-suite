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
!*  - defined assignment a1 = a2 where a1 & a2 are ptr of type DT of diff rank
!*  - subourtine for = is module procedure
!*
!234567890123456789012345678901234567890123456789012345678901234567890
            module m
		type base
		    integer, pointer :: id(:)
		end type

		interface assignment(=)
		    module procedure assgnPtr
		end interface

		contains
    	            subroutine assgnPtr(p1, p2)
		 	type(base), pointer, intent(inout) :: p1(:,:)
			class(base), target, intent(in) :: p2(:)

			p1(1:10,-10:-1) => p2
		    end subroutine

	    end module

        program main
                use m

		type(base), pointer :: p1(:,:)
		class(base), pointer :: p2(:)
		type(base), target :: tar1(400)
		integer, target, allocatable ::  t(:,:)

		allocate(t(400,2))

		do i = 1, 400
		    t(i,:) =(/ i, i+ 1 /)
		enddo

		tar1 =  (/(base( t(i,:) ),i=1,400)/)

		i = 51

		p2(tar1(i)%id(1):tar1(199)%id(2)) => tar1(i:)

		p1 = p2

		if ( .not. associated(p1)) error stop 11

		if (any(lbound(p1) .ne. (/1, -10/))) error stop 21
		if (any(ubound(p1) .ne. (/10, -1/))) error stop 23

		print *, (/ ((p1(i,j)%id, i=lbound(p1,1),ubound(p1,1)), &
			 j= lbound(p1,2),ubound(p1,2)) /)

        End program

