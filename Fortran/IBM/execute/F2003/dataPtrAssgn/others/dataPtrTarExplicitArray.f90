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
!* - data-target is array section of an explicit-shape dummy argument
!* - the shape of dummy arg differs from that of actual arg
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mod

    integer, pointer :: p(:,:)
    integer, parameter :: m=3, n=2

    contains
        subroutine sub(a)
            integer, target :: a(5,5)

            p(n:, m:) => a(::2,::2)
        end subroutine

end module

program main
use mod

    integer, target :: t(25)

    t = (/(i,i=1,25) /)

    call sub(t)

    if ( .not. associated(p) ) stop 3
    if ( any ( lbound(p) .ne. (/2, 3/) ) ) stop 11
    if ( any ( ubound(p) .ne. (/4, 5/) ) ) stop 15

    p = P * 10

    print *, p
    print *, t

end program
