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
!* - data-ptr of a component of sequence derived-type, character(:)
!* - data-ptr has bounds-remapping
!* - test operator ==
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

    type base
        character(:), pointer :: ptr(:)
    end type

    class(*), pointer :: b1

    type(base), target, allocatable :: b2

    character(4), target :: tar(4) =  (/ '1234', 'ABCD', '5678','OPQR' /)

    allocate(b2, source = base(tar) )

    b1 => b2

    select type (b1)
    	type is (base)
    	    b1%ptr(2:4) => b2%ptr
     	    if ( .not. associated(b1%ptr, b2%ptr) ) error stop 2
            if ( lbound(b1%ptr,1) /= 2 ) error stop 5
            if ( ubound(b1%ptr,1) /= 4 ) error stop 8

	    print *, b1%ptr
            if ( any(b1%ptr == b1%ptr .neqv. .true.) ) error stop 10

        class default
 	    stop 1
    end select

 End program

