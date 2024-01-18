! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-29
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - test bound-remapping,
!*                                   LHS is 1-dim,RHS is 2-dim
!*                               -
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

    integer, pointer :: p1(:)

    interface
        subroutine sub(x)
            integer, contiguous, pointer :: x(:,:)
        end subroutine
    end interface
end module


program main

    use mod

    integer, pointer, contiguous :: p2(:,:)

    allocate(p2(3,3), source=reshape((/3,5,7,9,11,13,17,19,23/),(/3,3/)))

    call sub(p2)

    if ( any (p1 .ne. (/3,5,7,9,11,13,17,19,23/))) stop 11

end program

subroutine sub(x)

    use mod, only : p1

    integer, contiguous, pointer :: x(:,:)

    p1(1:9) => x

end subroutine

