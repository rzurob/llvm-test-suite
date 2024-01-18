!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrLTChar.f
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

    if ( .not. associated(ptr, tar)) stop 1

    allocate(ch1(2), source = (/ "IBM XLF ", "ibm xlf "/) )

    ptr1(2:) => ch1(2:1:-1)
    if ( .not. associated(ptr1, ch1(2:1:-1))) stop 3
    if ( lbound(ptr1, 1) /= 2 ) stop 5
    if ( ubound(ptr1, 1) /= 3 ) stop 7

    call sub

    if ( .not. associated(ptr, ch(2:1:-1))) stop 11
    if ( lbound(ptr, 1) /= 10 ) stop 12
    if ( ubound(ptr, 1) /= 11 ) stop 13

    print *, ch1
    print *, ptr1
    print *, ptr

    contains
        subroutine sub
            use B, ch1 => ch
            use B,  ptr1 => ptr

            ptr1(10:11) => ch1(2:1:-1)

            if ( .not. associated(ptr1, ch1(2:1:-1))) stop 21
            if ( .not. associated(ptr, ch1(2:1:-1))) stop 22
            if ( lbound(ptr, 1) /= 10 ) stop 23
            if ( ubound(ptr, 1) /= 11 ) stop 24

            print *, ch1
            print *, ptr
            print *, ptr1

        end subroutine
end program
