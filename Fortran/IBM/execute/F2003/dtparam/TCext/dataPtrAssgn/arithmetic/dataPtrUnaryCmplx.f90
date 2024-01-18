! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrUnaryCmplx.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrUnaryCmplx.f 
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
!* - test unary operations +/- for type complex*8,16,32
!* - data-target is allocatable array. target is redefined by intrinisic
!*      assignment with same shape following pointer assignment 
!* - module defined in a seperate file 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  program main
	use m

 	type(B(20,4,8,16)), target :: b1 
 
        allocate(b1%c4(kind(b1%c4)), source=(/(cmplx(i,i,kind(b1%c4)), &
                     i=1,kind(b1%c4) )/))
        b1%ptr(1:kind(b1%c4)) => b1%c4(::1)


	! allocatable enhancement  
	b1%c4 = (/(cmplx(i,i+kind(b1%c4),kind(b1%c4)), i=1,kind(b1%c4) )/)

	if ( .not. associated(b1%ptr, b1%c4(::1))) stop 1
	if ( lbound(b1%ptr,1) /= 1) stop 2
	if ( ubound(b1%ptr,1) /= 4) stop 3 

	call output(b1, 4)

        allocate(b1%c8(kind(b1%c8)), source=(/(cmplx(i,i,kind(b1%c8)), &
                 i=1,kind(b1%c8) )/))

        b1%ptr(int(b1%c8(1)):) => b1%c8(8:1:-1)
         
	! allocatable enhancement  
	b1%c8 = (/(cmplx(i,i+kind(b1%c8),kind(b1%c8)), i=1,kind(b1%c8) )/)

	if ( .not. associated(b1%ptr, b1%c8(8:1:-1))) stop 4 
	if ( lbound(b1%ptr,1) /= 1) stop 5 
	if ( ubound(b1%ptr,1) /= 8) stop 6 

	call output(b1, 8)

        allocate(b1%c16(kind(b1%c16)), source=(/(cmplx(i,i,kind(b1%c16)), &
             i=1,kind(b1%c16) )/))

        b1%ptr(int(b1%c16(16)):) => b1%c16(::2)

	! allocatable enhancement  
	b1%c16 = (/(cmplx(i,i+kind(b1%c16),kind(b1%c16)), i=1,kind(b1%c16) )/)

	if ( .not. associated(b1%ptr, b1%c16(::2))) stop 7 
	if ( lbound(b1%ptr,1) /= 16) stop 8 
	if ( ubound(b1%ptr,1) /= 23) stop 9 

	call output(b1, 16)

        contains
	    subroutine output(arg, i)
		type(B(*,4,8,16)) arg
		integer i
	
		select type (x => arg%ptr)
	    	    type is (complex(4))
		 	write (*, '("(",f10.8,", ", f10.8, ")")') +x
	    	    type is (complex(8))
		 	write (*, '("(",f15.10,", ", f15.10, ")")') -x
	    	    type is (complex(16))
		 	write (*, '("(",f20.15,", ", f20.15, ")")') -x
	            class default
		        call zzrc(i)  
	        end select 

	   end subroutine
  end program

