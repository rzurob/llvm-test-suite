! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/others/dataPtrFuncnamebound.f
! opt variations: -qnol -qnodeferredlp

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
!* - module function result as lb of data-pointer
!* - one element of data-pointer as ub of the data-pointer
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   module m

	type t(n1,k1)    ! (20,4)
	    integer, kind        :: k1
	    integer, len         :: n1
	    integer(k1), pointer :: p(:)
	end type

        contains

            function get_lb(ptr)
                integer get_lb
                integer :: ptr(:)
		get_lb = lbound(ptr,1) + 3

            end function
   end module

   program main
        use m

	type(t(:,4)), allocatable :: aT

	allocate(t(20,4) :: aT)
	if ( .not. allocated(aT) ) error stop 1

	allocate(at%P(55), source=(/(i+2, i=1,55)/) )

	at%P(get_lb(at%P):at%P(10) ) => at%P(55::-1)
	if ( .not. associated(at%P) ) error stop 3

	if (lbound(at%p,1) /= 4) error stop 5
	if (ubound(at%p,1) /= 12) error stop 7

	if ( any(at%p /= (/(i,i=57,49,-1)/))) error stop 11
   End program