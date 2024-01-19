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
!* - data-ptr has bounds-spec-list
!* - test operator .eq.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base
	sequence
        character(:), pointer :: ptr(:,:)
    end type

end module

program main

    use m

    type(base), pointer :: b1(:)

    character(3), target, volatile :: tar(3,4) = reshape( &
         (/ '123', '456','789','abc','def','ghi','jkl', 'opq', &
           '012', '345', '678', 'zyx' /), (/3,4/) )

     allocate(b1(2), source= (/ base(null()), base(tar(3:1:-1,4:1:-1)) /) )

     b1(1)%ptr(len(tar(1,2)):,1:) => b1(2)%ptr

     if ( .not. associated(b1(1)%ptr, b1(2)%ptr) ) error stop 2
     if ( any ( lbound(b1(1)%ptr) .ne. (/ 3, 1 /) )) error stop 5
     if ( any ( ubound(b1(1)%ptr) .ne. (/ 5, 4 /) )) error stop 8

     print *, b1(1)%ptr

     if ( any(b1(1)%ptr .eq. b1(2)%ptr .neqv. .true.) ) error stop 10

 End program

