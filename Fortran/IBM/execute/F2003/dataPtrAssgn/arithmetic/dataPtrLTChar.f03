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
!* - data-ptr associated with a target before calling to an internal proc
!*      the name of data-ptr becomes a renamed local name
!* - verify data-ptr after procedure call
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module A
    character(8), target :: ch(2) = (/ 'compiler', 'COMPILER' /)
end module

module B
    use A
    character(:), pointer :: ptr(:)

end module

program main
    use B

    character(8), target, allocatable :: ch1(:)
    character(8), target :: tar(2) = (/ "IBM XLC ", 'ibm xlc ' /)
    character(8), pointer ::  ptr1(:)

    ptr => tar

    if ( .not. associated(ptr, tar)) error stop 1

    allocate(ch1(2), source = (/ "IBM XLF ", "ibm xlf "/) )

    ptr1(2:) => ch1(2:1:-1)
    if ( .not. associated(ptr1, ch1(2:1:-1))) error stop 3
    if ( lbound(ptr1, 1) /= 2 ) error stop 5
    if ( ubound(ptr1, 1) /= 3 ) error stop 7

    call sub

    if ( .not. associated(ptr, ch(2:1:-1))) error stop 11
    if ( lbound(ptr, 1) /= 10 ) error stop 12
    if ( ubound(ptr, 1) /= 11 ) error stop 13

    print *, ch1
    print *, ptr1
    print *, ptr

    contains
        subroutine sub
            use B, ch1 => ch
            use B,  ptr1 => ptr

            ptr1(10:11) => ch1(2:1:-1)

            if ( .not. associated(ptr1, ch1(2:1:-1))) error stop 21
            if ( .not. associated(ptr, ch1(2:1:-1))) error stop 22
            if ( lbound(ptr, 1) /= 10 ) error stop 23
            if ( ubound(ptr, 1) /= 11 ) error stop 24

            print *, ch1
            print *, ptr
            print *, ptr1

        end subroutine
end program