! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr032a5_1.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr032a5_1.f
! %VERIFY: fconstr032a5_1.out:fconstr032a5_1.vf
! %STDIN:
! %STDOUT: fconstr032a5_1.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 10/14/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
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

module m
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure :: print => printData
    end type

    type container(k2,n2)    ! (4,20)
        integer, kind                       :: k2
        integer, len                        :: n2
        integer(k2)                            id
        class(dataType(k2,n2)), allocatable :: data(:)
    end type

    contains

    subroutine printData (d)
        class (dataType(4,*)), intent(in) :: d

        print *, 'dataType'
    end subroutine
end module

module m1
use m, only : dataType
    type, extends(dataType) :: realData(k3)    ! (4,20,8)
        integer, kind :: k3
        real(k3)      :: data

        contains

        procedure, pass(d) :: print => printRealData
    end type

    contains

    subroutine printRealData (d)
        class(realData(4,*,8)), intent(in) :: d

        write (*, '(g10.3)') d%data
    end subroutine
end module


program fconstr032a5_1
use m
use m1
    class (dataType(4,20)), pointer :: d1(:), d2(:,:)

    type (realData(4,20,8)), target :: r1(-1:0), r2(-1:0, 2)

    d1 => r1
    d2 => r2

    r1%data = (/-1.0, 1.5/)

    r2%data = reshape ((/-1.5, -.5, .5, 2.5/), (/2,2/))

    call associate1 (x = container(4,20)(10, r1))

    call associate2 (container(4,20)(20, r2(0,:)))

    contains

!    associate (x => container(4,20)(10, r1))
    subroutine associate1 (x)
        type(container(4,*)), intent(in) :: x

        if (.not. allocated (x%data)) error stop 1_4

        if ((lbound(x%data,1) /= -1) .or. (ubound(x%data,1) /= 0)) error stop 2_4

        if (x%id /= 10) error stop 4_4
        call x%data(-1)%print
        call x%data(0)%print
    end subroutine

!    associate (x => container(4,20)(20, r2(0,:)))
    subroutine associate2 (x)
        type(container(4,20)), intent(in) :: x

        if (x%id /= 20) error stop 5_4

        if (.not. allocated (x%data)) error stop 6_4
        if ((lbound(x%data,1) /= 1) .or. (ubound(x%data,1) /= 2)) error stop 7_4

        call x%data(1)%print
        call x%data(2)%print
    end subroutine
end
