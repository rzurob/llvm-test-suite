!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrallocEnh1.f
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
!* - data-tar is an allocatable component, which is redefined by intrinisc =,
!*   due to  the len type parameter changed. Test the association status
!*   of data-ptr &data-target
!* 
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base 
        character(:), allocatable :: ch(:)
  	class(*), pointer :: p(:)	
    end type

end module

program main

    use m

    type(base), target :: b1

    allocate(b1%ch(10), source=(/ ( repeat(achar(i),2), i=65,74)/))

    b1%p(-100:-91) => b1%ch
    if (.not. associated(b1%p,b1%ch)) stop 1
    if ( lbound(b1%p,1) /= -100) stop 2
    if ( ubound(b1%p,1) /= -91) stop 3 

    select type (x=>b1%p) 
	type is (character(*))
            if ( any(x .ne. (/ ( repeat(achar(i),2), i=65,74)/))) stop 5
 	class default
	    stop 6	
    end select

    b1%ch = (/ ( repeaT(ACHar(i),3), i=65,74)/)

    if ( any(b1%ch .ne. (/ ( repeat(achar(i),3), i=65,74)/))) stop 7 
    if ( associated(b1%p,b1%ch)) stop 8 

end program

