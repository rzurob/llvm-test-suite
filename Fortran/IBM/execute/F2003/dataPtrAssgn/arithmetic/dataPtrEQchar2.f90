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
!* - data-tar is of type deferred char with allocatable attr, module var
!* - data-ptr is of type nondeferred char, module var
!* - data pointer assignment is used in module sub
!* - track defect 321721
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    character(:), allocatable :: ch1(:)
    target ch1

    character(7), pointer :: ptr(:)

    contains

        subroutine sub(i)
            integer i

            ptr(2:i) => ch1(ubound(ch1,1):lbound(ch1,1):-1)
        end subroutine
end module

program main
    use m

    allocate (ch1(2), source = (/ '1234567', 'abcdefg' /) )

    call sub(3)

    if ( .not. associated(ptr)) error stop 1
    if ( lbound(ptr,1) /=2 ) error stop 2
    if ( ubound(ptr,1) /=3 ) error stop 3
    if ( any (ptr /= (/'abcdefg', '1234567'/)) ) error stop 5

end program
