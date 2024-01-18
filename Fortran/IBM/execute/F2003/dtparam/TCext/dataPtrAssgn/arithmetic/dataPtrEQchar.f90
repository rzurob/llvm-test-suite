! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrEQchar.f
! opt variations: -qck -qnok -ql

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrEQchar.f
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
!* - data-ptr has bounds-spec-list
!* - test operator .eq.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base(k1)    ! (4)
        integer, kind :: k1
	sequence
        character(:), pointer :: ptr(:,:)
    end type

end module

program main

    use m

    type(base(4)), pointer :: b1(:)

    character(3), target, volatile :: tar(3,4) = reshape( &
         (/ '123', '456','789','abc','def','ghi','jkl', 'opq', &
           '012', '345', '678', 'zyx' /), (/3,4/) )

     allocate(b1(2), source= (/ base(4)(null()), base(4)(tar(3:1:-1,4:1:-1)) /) )

     b1(1)%ptr(len(tar(1,2)):,1:) => b1(2)%ptr

     if ( .not. associated(b1(1)%ptr, b1(2)%ptr) ) stop 2
     if ( any ( lbound(b1(1)%ptr) .ne. (/ 3, 1 /) )) stop 5
     if ( any ( ubound(b1(1)%ptr) .ne. (/ 5, 4 /) )) stop 8

     print *, b1(1)%ptr

     if ( any(b1(1)%ptr .eq. b1(2)%ptr .neqv. .true.) ) stop 10

 End program

