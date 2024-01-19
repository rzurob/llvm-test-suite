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
!* - data-pointer, a component of type class(*), dynamic type is character(*)
!* - lbound is intrinsic len
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base
        class(*), pointer :: up(:,:)
    end type

end module

program main
    use m

	type(base), allocatable :: b

	allocate(b)
	allocate(b%up(2,2), source=reshape( (/'ab','12','ef','56'/),(/2,2/)) )

	b%up(len('IBM', len('abcd')):, ior(2,3):) => b%up
        if ( .not. associated(b%up)) error stop 45
        if ( any ( lbound(b%up) .ne. (/3,3/) )) error stop 47
        if ( any ( ubound(b%up) .ne. (/4,4/) )) error stop 49

        select type(x => b%up)
            type is (character(*))
		print *, x
		print *, minloc(x)
            class default
                stop 53
        end select

    end program
