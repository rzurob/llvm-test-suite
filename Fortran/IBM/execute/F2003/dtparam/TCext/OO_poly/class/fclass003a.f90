! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass003a.f
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
! %GROUP: fclass003a.f
! %VERIFY: fclass003a.out:fclass003a.vf
! %STDIN:
! %STDOUT: fclass003a.out
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
!*  DATE                       : 07/05/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : CLASS keyword (defined assignment and elemental
!                               final binding)
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
        integer, kind        :: k1
        integer(k1), pointer :: data(:) => null()

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    elemental subroutine finalizeBase(b)
        type (base(4)), intent(inout) :: b

        if (associated (b%data)) deallocate (b%data)
    end subroutine
end module

use m
    interface assignment (=)
        subroutine baseAssgn2Base (a, b)
        use m
            class (base(4)), intent(out) :: a
            type (base(4)), intent(in) :: b

        end subroutine
    end interface

    type (base(4)) b1, b2
    type (child(4,1,20)) c1

    allocate (b1%data(10))

    b1%data = (/(11-i, i=1, 10)/)

    b2 = b1

    if (associated (b2%data, b1%data)) error stop 2_4

    print *, b2%data

    c1 = b1

    if (associated (c1%data, b1%data)) error stop 3_4

    print *, c1%data

    c1 = b2

    if (associated (c1%data, b2%data)) error stop 3_4

    print *, c1%data

    deallocate (b1%data, b2%data, c1%data)
end

subroutine baseAssgn2Base (a, b)
use m
    class (base(4)), intent(out) :: a
    type (base(4)), intent(in) :: b

    if (associated (a%data)) error stop 1_4

    allocate (a%data(size(b%data)))

    a%data = b%data
end subroutine
