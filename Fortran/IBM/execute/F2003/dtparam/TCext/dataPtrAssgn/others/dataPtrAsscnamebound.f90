! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/F2003/dataPtrAssgn/others/dataPtrAsscnamebound.f
! opt variations: -qnok -ql -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrAsscnamebound.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - lb/ub are the associate names of associate/select-type constructs
!* - lb/ub are the elements of array
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    program main
	type base(k1)    ! (4)
	    integer, kind :: k1
	end type

	type, extends(base) :: child    ! (4)
	    integer(k1), pointer :: id
	end type

	class(*), pointer :: lb
	integer*2, save  :: ub(3)
	integer, target :: t(256)

	class(base(4)), pointer :: p(:,:)
	type(child(4)), target :: tar(256)

        ub(1) = 128
        ub(2) =	64
	ub(3) = 32

	allocate(lb,source=50_8)

	t = (/ (i,i=1,256) /)
	tar = (/ ( child(4)(t(i)), i=1,256) /)

	select type ( x => lb)
	    type is (integer*8)
		associate( y => ub)
                    p(x:y(2),y(1)/y(3):15) => tar(256::-1)
                end associate
	    class default
		stop 21
 	end select

	if ( .not. associated(p)) stop 23
	if ( any(lbound(p) .ne. (/50, 4/))) stop 31
	if ( any(ubound(p) .ne. (/64, 15/))) stop 33

	select type (p)
	    type is (child(4))
		print *, (/ ((p(i,j)%id, i=lbound(p,1),ubound(p,1)), j=lbound(p,2), ubound(p,2)) /)
	    class default
		stop 51
	end select


end program
