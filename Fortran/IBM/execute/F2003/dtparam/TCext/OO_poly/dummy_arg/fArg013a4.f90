! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg013a4.f
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
! %GROUP: fArg013a4.f
! %VERIFY: fArg013a4.out:fArg013a4.vf
! %STDIN:
! %STDOUT: fArg013a4.out
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
!*  DATE                       : 05/18/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (TARGET dummy-arg for
!                               assumed-shape array associated with whole array
!                               that is with TARGET attribute)
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
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    class (*), pointer :: x(:) => null()
    class (base(4)), pointer :: b1_m (:)

    contains

    logical function associatedwithX (b)
        class (base(4)), target, intent(inout) :: b(2:)

        associatedwithX = associated (x, b)

        if (associatedwithX) b1_m => b

        b%id = b%id + 20
    end function

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg013a3
use m
    class (base(4)), pointer :: b1 (:)
    type (child(4,1,20)) :: c1 (3)

    c1 = (/child(4,1,20) (1,'c1_1'), child(4,1,20)(2,'c1_2'), child(4,1,20) (3,'c1_3')/)

    allocate (b1(3:5), source=c1)

    x => b1

    if (.not. associatedwithX (b1)) error stop 1_4

    if (.not. associated (x, b1_m)) error stop 2_4

    if (.not. associated (b1_m, b1)) error stop 3_4

    if ((lbound(b1_m,1) /= 2) .or. (size (b1_m) /= 3)) error stop 4_4

    if ((lbound(x,1) /= 3) .or. (size (x) /= 3)) error stop 5_4

    call b1_m(2)%print
    call b1_m(3)%print
    call b1_m(4)%print

    deallocate (b1)
end
