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
!*  - defined assignment a1 = a2 where a1 & a2 are of type DT
!*  - DT has the pointer component
!*  - pointer assignment appears in the subroutine of defined assignment
!*
!234567890123456789012345678901234567890123456789012345678901234567890
            module m
		type A
		    class(*), pointer :: p(:,:)
		end type

		interface assignment(=)
		    subroutine assgnPtr(p1, p2)
			import A
		 	type(A), intent(inout) :: p1
			type(A), intent(in) :: p2
		    end subroutine
		end interface

		contains
		    subroutine output(a)
			class(*), pointer :: a(:,:)

			select type(x => a)
        	            type is (integer)
                                print *, x
				if (any(lbound(x) .ne. (/10,30/))) error stop 21
				if (any(ubound(x) .ne. (/29,49/))) error stop 22
        	            type is (integer*8)
                                print *, x
				if (any(lbound(a) .ne. (/-1,0/))) error stop 27
				if (any(ubound(a) .ne. (/1,5/))) error stop 28
                            class default
                                stop 31
                        end select

		    end subroutine
	    end module

        program main
                use m

		type(A) :: a1, a2

		integer, target :: tar1(400)
		integer*8, target :: tar2(400)

		tar1 = (/ (/(i,i=1,200,2)/), (/(i,i=2,200,2 )/), &
		 (/(i, i=201,400,2)/), (/(i,i=202,400,2)/) /)

		a2%p(1:20,1:20) => tar1

		if ( .not. associated(a2%p)) error stop 2
		if ( any(lbound(a2%p) .ne. (/1,1/))) error stop 3
		if ( any(ubound(a2%p) .ne. (/20,20/))) error stop 5

		a1 = a2
		if ( .not. associated(a1%p)) error stop 11
		call output(a1%p)

		tar2 = int(tar1,8)
		a2%p(1:20,1:20) => tar2

		if ( .not. associated(a2%p)) error stop 12
		if ( any(lbound(a2%p) .ne. (/1,1/))) error stop 13
		if ( any(ubound(a2%p) .ne. (/20,20/))) error stop 15

		a1 = a2
		if ( .not. associated(a1%p)) error stop 23
		call output(a1%p)


        End program

    subroutine assgnPtr(a1, a2)
	use m, only : A
 	type(A), intent(inout) :: a1
	type(A), intent(in) :: a2

	if ( .not. associated(a2%p) ) error stop 41

	select type ( x=> a2%p)
	    type is (integer)
		a1%p(10:,30:) => a2%p
	    type is (integer*8)
		a1%p(-1:1, 0:5) => a2%p(:,2)
	    class default
	   	stop 42
	end select

    end subroutine
