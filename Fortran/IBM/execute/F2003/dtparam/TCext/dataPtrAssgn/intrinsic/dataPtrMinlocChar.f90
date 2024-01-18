! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrMinlocChar.f
! opt variations: -qnok -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrMinlocChar.f 
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
!* - data-pointer, a component of type class(*), dynamic type is character(*)
!* - lbound is intrinsic len
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), pointer :: up(:,:)
    end type

end module

program main
    use m

	type(base(4,:)), allocatable :: b

	allocate(base(4,20) :: b)
	allocate(b%up(2,2), source=reshape( (/'ab','12','ef','56'/),(/2,2/)) )

	b%up(len('IBM', len('abcd')):, ior(2,3):) => b%up
        if ( .not. associated(b%up)) stop 45 
        if ( any ( lbound(b%up) .ne. (/3,3/) )) stop 47 
        if ( any ( ubound(b%up) .ne. (/4,4/) )) stop 49

        select type(x => b%up)
            type is (character(*))
		print *, x
		print *, minloc(x)
            class default
                stop 53 
        end select

    end program
