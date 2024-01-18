! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a11.f
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
! %GROUP: fArg005a11.f
! %VERIFY: fArg005a11.out:fArg005a11.vf
! %STDIN:
! %STDOUT: fArg005a11.out
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
!*  DATE                       : 05/05/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (poly-pointer dummy-arg
!*                               used in the type bound; arrays)
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
        procedure, non_overridable :: copyData => copyBaseData
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

    subroutine copyBaseData (b, bArray, arraySize)
        class (base(4)), intent(in) :: b
        class (base(4)), intent(out), pointer :: bArray(:)
        integer*4, intent(in) :: arraySize

        allocate (bArray (arraySize), source=b)
    end subroutine
end module

program fArg005a11
use m
    type (base(4)) :: b1 = base(4) (10)
    type (child(4,1,20)) :: c1

    class (base(4)), pointer :: b2(:), b3(:)

    c1%id = 20

    call b1%copyData (b2, 3)

    if (size (b2) /= 3) error stop 1_4

    do i = 1, 3
        call b2(i)%print
    end do


    call c1%copyData (b3, 2)

    if (size (b3) /= 2) error stop 2_4

    call b3(1)%print
    call b3(2)%print

    deallocate (b2, b3)
end
