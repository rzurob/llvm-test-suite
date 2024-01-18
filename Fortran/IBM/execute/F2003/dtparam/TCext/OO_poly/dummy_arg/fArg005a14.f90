! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a14.f
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
! %GROUP: fArg005a14.f
! %VERIFY: fArg005a14.out:fArg005a14.vf
! %STDIN:
! %STDOUT: fArg005a14.out
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
!*  DATE                       : 05/06/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (nonpoly-pointer dummy-arg
!*                               used in pointer assignment)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine associate (b, b1)
        type (base(4)), pointer, intent(out) :: b
        class (base(4)), target, intent(in) :: b1

        b => b1
    end subroutine

    subroutine associateArray (b, b1)
        type (base(4)), pointer, intent(out) :: b(:)
        class (base(4)), target, intent(in) :: b1(:)

        b => b1
    end subroutine
end module


program fArg005a14
use m
    type (base(4)), pointer :: b1, b2 (:)

    type (child(4,1,20)), target :: c1, c2 (3:5)
    type (child(4,1,20)) :: c3 (2)

    class (base(4)), allocatable, target :: b3, b4(:)

    c1 = child(4,1,20) (1, 'c1_target')

    call associate (b1, c1)

    if (.not. associated (b1, c1%base)) error stop 1_4

    call b1%print

    allocate (b3, source=child(4,1,20) (2, 'b3_allocatable'))

    call associate (b1, b3)

    !! b1 and b3 are not associated as b1 only associated with part of b3
    if (associated (b1, b3)) error stop 2_4

    call b1%print

    !! done with scalars

    call associateArray (b2, c2)

    if (.not. associated (b2, c2%base)) error stop 3_4

    if (size (b2) /= 3) error stop 4_4

    if ((lbound(b2,1) /= 1) .or. (ubound(b2,1) /= 3)) error stop 5_4

    c2%id = (/3,4,5/)
    c2%name = (/'c2_target_3', 'c2_target_4', 'c2_target_5'/)

    call b2(1)%print
    call b2(2)%print
    call b2(3)%print

    c3%id = (/6,7/)
    c3%name = (/'b4_allocatable_6', 'b4_allocatable_7'/)

    allocate (b4 (6:7), source=c3)

    if (lbound(b4, 1) /= 6) error stop 6_4

    call associateArray (b2, b4)

    if (size(b2) /= 2) error stop 7_4

    if ((lbound(b2,1) /= 1) .or. (ubound(b2,1) /= 2)) error stop 8_4

    call b2(1)%print
    call b2(2)%print
end
