! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrEQchar1.f
! opt variations: -qnock -qnok -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrEQchar1.f
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

    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        character(:), pointer :: ptr(:)
    end type

    class(*), pointer :: b1

    type(base(4,:)), target, allocatable :: b2

    character(4), target :: tar(4) =  (/ '1234', 'ABCD', '5678','OPQR' /)

    allocate(b2, source = base(4,20)(tar) )

    b1 => b2

    select type (b1)
    	type is (base(4,*))
    	    b1%ptr(2:4) => b2%ptr
     	    if ( .not. associated(b1%ptr, b2%ptr) ) stop 2
            if ( lbound(b1%ptr,1) /= 2 ) stop 5
            if ( ubound(b1%ptr,1) /= 4 ) stop 8

	    print *, b1%ptr
            if ( any(b1%ptr == b1%ptr .neqv. .true.) ) stop 10

        class default
 	    stop 1
    end select

 End program

