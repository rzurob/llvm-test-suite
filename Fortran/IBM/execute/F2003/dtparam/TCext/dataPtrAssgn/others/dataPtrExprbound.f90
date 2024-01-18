! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/others/dataPtrExprbound.f
! opt variations: -qnok -ql

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
!* - lb/ub are arithmetic expr, operands are intrinsic name, pure funcname,
!*     const & variable.
!* - data-target is a component of DT
!* - data pointer/target of type byte
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    program main

	type A(k1)    ! (4)
	    integer, kind :: k1
	end type

	type, extends(A) :: B    ! (4)
            byte, allocatable :: t(:)
	end type

	type(B(4)), pointer :: b1

	byte, pointer :: P(:,:)

	allocate(b1)
	if ( .not. associated(b1) ) error stop 3

	allocate(b1%t(25), source= (/ (/(int(i,1), i = 25,11,-1)/), &
		(/(int(i,1),i= 10,1,-1 )/) /) )
	if ( .not. allocated(b1%t)) error stop 5

	call sub(p,b1)

	if ( .not. associated(p)) error stop 7
	if ( any(shape(p) .eq. (/99,99 /) )) error stop 11

	if ( any(lbound(p) .ne. (/3, 1/))) error stop 31
	if ( any(ubound(p) .ne. (/8, 2/))) error stop 33
	print *, p

	contains
	    pure subroutine sub(p, arg)
	        class(A(4)), intent(in), target :: arg
	        byte, intent(inout), pointer :: p(:,:)

		select type (arg)
		    type is (B(4))
			p(len('IBM'):arg%t(3)/5+kind(4),'10'o-7:func()*B'10'-2) => arg%t
		    class default
			allocate(P(99,99))
		end select

	    end subroutine

            pure function func()
		integer func
		func = 2
	    end function

 End program
