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
!* - a module object is renamed in an internal proc with a local name that is
!*    used as data-tar
!* - data-ptr is a module object
!* - test the association status and value before/after calling to the proc
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

    character(8), target :: tar(2) = (/ "IBM XLF ", 'ibm xlf ' /)

    ptr(2:) => tar(2:1:-1)

    if ( .not. associated(ptr, tar(2:1:-1))) stop 1
    if ( lbound(ptr, 1) /= 2 ) stop 3
    if ( ubound(ptr, 1) /= 3 ) stop 5
    print *, ptr

    call sub

    if ( .not. associated(ptr, ch(2:1:-1))) stop 11
    if ( lbound(ptr, 1) /= 1 ) stop 13
    if ( ubound(ptr, 1) /= 2 ) stop 15
    print *, ptr

    contains
        subroutine sub
            use B, tar => ch

            ptr(lbound(tar,1):ubound(tar,1)) => tar(2:1:-1)
            if ( .not. associated(ptr, tar(2:1:-1))) stop 21
            if ( lbound(ptr, 1) /= 1 ) stop 23
            if ( ubound(ptr, 1) /= 2 ) stop 25
            print *, ptr
	    print *, ptr >= (/'COMPILER', 'COMPILER' /)

        end subroutine
end program
