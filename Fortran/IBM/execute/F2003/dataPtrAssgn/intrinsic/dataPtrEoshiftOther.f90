!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrEoshiftOther.f
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
!* - date-targets are explicit-shape arrays ( automatic, adjustable arrays)
!* - data-targets are of types real, integer, dbl complx
!* - data_pointers are of class(*)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

	class(*), pointer :: p(:,:)

	contains

        subroutine intsub(x,y,tar)
	    integer :: x, y 
            integer, target :: tar(x,y)

	    p(1:5,1:1) => tar(:,5)

            if ( .not. associated(p)) stop 5
            if ( any (lbound(p) .ne. (/1,1/) )) stop 7 
            if ( any (ubound(p) .ne. (/5,1/) )) stop 9

	    select type (p)
	        type is (integer)
		    p = eoshift(p, -2, -1)
	 	class default
		    stop 12 
	    end select

        end subroutine

	subroutine realsub(ub)
	    integer ub
	    real, target :: tar(ub/2,ub/2) 

	    do i = 1, ub/2
		do j = 1, ub/2
		    tar(j,i) = i*10 + j 
	        end do
	    enddo

	    p(1:,1:) => tar 

            if ( .not. associated(p)) stop 15
            if ( any (lbound(p) .ne. (/1,1/) )) stop 17 
            if ( any (ubound(p) .ne. (/5,5/) )) stop 19

	    select type (p)
	        type is (real)
		    write(*, '(5f8.2)') eoshift(p, 1, 2.0, 2) 
	 	class default
		    stop 22 
	    end select

	end subroutine

	subroutine dbcmplxsub(x,tar)
	    integer x
	    double complex, target :: tar(x+x, x*3) 

	    p(x:,x*3:) => tar

            if ( .not. associated(p)) stop 25
            if ( any (lbound(p) .ne. (/x, x*3/) )) stop 27 
            if ( any (ubound(p) .ne. (/3*x-1,x*6-1/) )) stop 29

	    select type (p)
	        type is (complex(8))
	 	    write (*, '("(",f10.6,", ", f10.6, ")")') eoshift(p,-1, &
			(2.0D+01,1.0D+01), 1 )
	 	class default
		    stop 32 
	    end select

	end subroutine

end module

program main

        use m

        integer :: i4(10,10)
	complex*16 :: c16(6,6)

        i4 = reshape( (/ (i,i=1,100) /), (/10,10/))

        call intsub(5,5,i4)

	print *, i4

        call realsub(10) 

	do j = 1, 6 
	    do i = 1, 6 
		c16(i,j) = cmplx(i,j,8)
	    enddo
	enddo 

	call dbcmplxsub(2, c16)

 end program
