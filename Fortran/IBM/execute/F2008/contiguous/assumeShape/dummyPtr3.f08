! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-29
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - actual arg is module array pointer
!*                               - array pointer with CONTIGUOUS attr
!*                                 is dummy arg of internal procedure.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
    integer, pointer, contiguous :: p2(:)
end module

program main

    use mod

    integer, pointer :: p1(:)

    allocate(p2(9), source=(/3,5,7,9,11,13,17,19,23/))

    call sub(p2)

    if ( any (p1 .ne. (/3,5,7,9,11,13,17,19,23/))) error stop 11

    contains
        subroutine sub(x)
            integer, contiguous, pointer :: x(:)
            p1 => x
        end subroutine

end program
