! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg504.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 04/07/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (miscellaneous)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 0

        contains

        procedure :: print => printBase
        procedure, nopass :: printType => printBaseType
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = ''

        contains

        procedure :: print => printChild
        procedure, nopass :: printType => printChildType
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

    subroutine printChildType (i)
        integer, intent(out) :: i

        i = 2
    end subroutine

    subroutine printBaseType (i)
        integer, intent(out) :: i

        i = 1
    end subroutine
end module

program fArg504
use m
    type (child(4,1,20)) :: c1 = child(4,1,20)(10, 'c1')

    call abc (c1)

    contains

    subroutine abc (cc)
        IMPLICIT class (base(4)) (c)

        call c1%print

        call cc%print
        call cc%printType (i1)

        if (i1 /= 2) error stop 1_4
    end subroutine
end
