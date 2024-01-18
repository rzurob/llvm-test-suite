! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr032a5.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr032a5.f
! %VERIFY: fconstr032a5.out:fconstr032a5.vf
! %STDIN:
! %STDOUT: fconstr032a5.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (poly-pointers as the
!                               data-source for poly-allocatable component in
!                               the structure constructor; use array components)
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

program fconstr032a5
use m
use m1
    class (dataType(4,20)), pointer :: d1(:), d2(:,:)

    type (realData(4,20,8)), target :: r1(-1:0), r2(-1:0, 2)

    d1 => r1
    d2 => r2

    r1%data = (/-1.0, 1.5/)

    r2%data = reshape ((/-1.5, -.5, .5, 2.5/), (/2,2/))

    call associate1 (x = container(4,20)(10, d1))

    call associate2 (container(4,20)(20, d2(0,:)))

    contains

!    associate (x => container(4,20)(10, d1))
    subroutine associate1 (x)
        type(container(4,*)), intent(in) :: x

        if (.not. allocated (x%data)) error stop 1_4

        if ((lbound(x%data,1) /= -1) .or. (ubound(x%data,1) /= 0)) error stop 2_4

        if (x%id /= 10) error stop 4_4
        call x%data(-1)%print
        call x%data(0)%print
    end subroutine

!    associate (x => container(4,20)(20, d2(0,:)))
    subroutine associate2 (x)
        type(container(4,20)), intent(in) :: x

        if (x%id /= 20) error stop 5_4

        if (.not. allocated (x%data)) error stop 6_4
        if ((lbound(x%data,1) /= 1) .or. (ubound(x%data,1) /= 2)) error stop 7_4

        call x%data(1)%print
        call x%data(2)%print
    end subroutine
end
