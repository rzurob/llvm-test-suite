! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/F2003/dataPtrAssgn/others/dataPtrDefineAssgn1.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrDefineAssgn1.f 
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
!*  - defined assignment a1 = a2
!*  - a1 and a2 are args of the subroutine of defined assignment, where
!*     a1 is of type DT with pointer component; a2 is an integer array
!*      
!234567890123456789012345678901234567890123456789012345678901234567890
            module m

		type base(k1)    ! (4)
		    integer, kind        :: k1
		    integer(k1), pointer :: p(:,:,:)
		end type

		type A(k2)    ! (4)
		    integer, kind                :: k2
		    class(base(k2)), allocatable :: next 
		end type

		interface assignment(=)
		    subroutine assgnPtr(a1,a2)
			import A
		 	type(A(4)), intent(inout) :: a1
			integer, intent(in) :: a2(:) 
		    end subroutine 
		end interface

	    end module

        program main
                use m

		type(A(4)) :: a1

		integer, target :: tar1(400)

		tar1 = (/ (/(i,i=1,200,2)/), (/(i,i=2,200,2 )/), &
		 (/(i, i=201,400,2)/), (/(i,i=202,400,2)/) /)

		allocate(a1%next)

		a1 = tar1

		if ( .not. associated(a1%next%p)) stop 11
		if ( any(lbound(a1%next%p) .ne. (/1,6,11/))) stop 13
		if ( any(ubound(a1%next%p) .ne. (/5,10,20/))) stop 15

		a1%next%p(1:,1:,1:) => a1%next%p

		if ( .not. associated(a1%next%p)) stop 11
		if ( any(lbound(a1%next%p) .ne. (/1,1,1/))) stop 13
		if ( any(ubound(a1%next%p) .ne. (/5,5,10/))) stop 15

		print *,a1%next%p
		
        End program

    subroutine assgnPtr(a1, a2)
	use m, only : A	
 	type(A(4)), intent(inout) :: a1
	integer, target, intent(in) :: a2(:)

	if ( .not. allocated(a1%next)) stop 41
	a1%next%p(1:5,6:10,11:20) => a2
    end subroutine 
