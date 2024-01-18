! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrTransferDT.f
! opt variations: -qnok -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrTransferDT.f 
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
!* - data-pointer  of poly type, type compatible with data-target
!* - as arg of transfer 
!* - data-target has allocatable attribute
!* - after ptr =, apply intrinsin = to data-target. tar = tar
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module m

    type base(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: id
    end type

    type, extends(base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
    end type

end module

    program main

        use m
        class(base(:,4)), pointer :: p(:,:)
        type(child(:,4,4,:)), target, allocatable :: tar(:), res(:)

        allocate(tar(64), source = (/ ( child(20,4,4,20)(i), i=1,64) /))

        p(1:8, 2:9) => tar

        if ( .not. associated(p)) stop 11
        if ( any(lbound(p) .ne. (/1,2/)) ) stop 13
        if ( any(ubound(p) .ne. (/8,9/)) ) stop 15

	tar = tar 

        select type (p)
            type is (child(*,4,4,*))
		do j = 2,9 
		    do i = 1, 8
                        print *, p(i,j)%id
		    enddo
		enddo
            class default
                stop 25
        end select

	res = transfer(p, (/ child(20,4,4,20)(99), child(20,4,4,20)(200) /) )

	print *, shape(res)

	do i = 1, 64 
	    print *, res(i)%id
	end do
end program
