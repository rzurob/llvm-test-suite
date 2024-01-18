!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrExprbound.f 
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
!*  DESCRIPTION
!*
!* - lb/ub are arithmetic expr, operands are intrinsic name, pure funcname, 
!*     const & variable.
!* - data-target is a component of DT 
!* - data pointer/target of type byte 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
 
    program main

	type A
	end type

	type, extends(A) :: B
            byte, allocatable :: t(:)
	end type

	type(B), pointer :: b1

	byte, pointer :: P(:,:)

	allocate(b1)
	if ( .not. associated(b1) ) stop 3

	allocate(b1%t(25), source= (/ (/(int(i,1), i = 25,11,-1)/), &
		(/(int(i,1),i= 10,1,-1 )/) /) )
	if ( .not. allocated(b1%t)) stop 5

	call sub(p,b1)

	if ( .not. associated(p)) stop 7 
	if ( any(shape(p) .eq. (/99,99 /) )) stop 11 

	if ( any(lbound(p) .ne. (/3, 1/))) stop 31
	if ( any(ubound(p) .ne. (/8, 2/))) stop 33
	print *, p

	contains
	    pure subroutine sub(p, arg)
	        class(A), intent(in), target :: arg
	        byte, intent(inout), pointer :: p(:,:)

		select type (arg)
		    type is (B)
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
