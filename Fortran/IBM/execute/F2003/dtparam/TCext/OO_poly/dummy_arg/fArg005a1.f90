! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a1.f
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
! %GROUP: fArg005a1.f
! %VERIFY: fArg005a1.out:fArg005a1.vf
! %STDIN:
! %STDOUT: fArg005a1.out
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
!*  DATE                       : 05/03/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (poly-allocatable array
!*                               dummy-arg associated)
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

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

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
end module

program fArg005a1
use m
    interface
        subroutine createBaseArray (b, id, name, arraySize)
        use m
            class (base(4)), allocatable, intent(out) :: b(:)
            integer*4, intent(in) :: id, arraySize
            character(*), intent(in), optional :: name
        end subroutine
    end interface

    class (base(4)), allocatable :: b1(:)

    call createBaseArray (b1, 1, 'b1_array', 3)

    if (size(b1) /= 3) error stop 1_4

    call b1(1)%print
    call b1(2)%print
    call b1(3)%print

    call createBaseArray (b1, 10, arraySize=2)

    if (size (b1) /= 2) error stop 2_4

    call b1(1)%print
    call b1(2)%print
end


subroutine createBaseArray (b, id, name, arraySize)
use m
    class (base(4)), allocatable, intent(out) :: b(:)
    integer*4, intent(in) :: id, arraySize
    character(*), intent(in), optional :: name


    if (present(name)) then
        allocate (b(arraySize), source=child(4,1,20)(id, name))
    else
        allocate (b(arraySize))
        b%id = id
    end if
end subroutine

