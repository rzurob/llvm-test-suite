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
! %GROUP: fArg005a2.f
! %VERIFY: fArg005a2.out:fArg005a2.vf
! %STDIN:
! %STDOUT: fArg005a2.out
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
!*  DATE                       : 05/04/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (allocatable nonpoly-dummy-arg
!*                               with INTENT(OUT) attribute)
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
    type base
        integer*4 :: id = -1

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    type, extends (base) :: child
        character*20 :: name = 'no-name'

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine createChild (c, id, name)
        type (child), allocatable, intent(out) :: c
        integer*4, optional, intent(in) :: id
        character(*), optional, intent(in) :: name

        allocate (c)

        if (present(id)) c%id = id
        if (present (name)) c%name = name
    end subroutine

    subroutine createBaseArray (b, id, arraySize)
        type (base), allocatable, intent(out) :: b (:)
        integer*4, intent(in) :: arraySize
        integer*4, intent(in), optional :: id

        allocate (b(arraySize))

        if (present(id))  b%id = id
    end subroutine
end module

program fArg005a2
use m
    type (child), allocatable :: c1
    type (base), allocatable :: b1(:)

    print *, 'begin'

    call createChild (c1)

    if ((c1%id /= -1) .or. (c1%name /= 'no-name')) error stop 1_4

    print *, 'before 2nd call'

    call createChild (c1, 1, 'c1')

    if ((c1%id /= 1) .or. (c1%name /= 'c1')) error stop 2_4

    print *, 'before 3rd call'

    call createChild (c1, name='c1_2')

    if ((c1%id /= -1) .or. (c1%name /= 'c1_2')) error stop 3_4

    print *, 'before 4th call'

    call createBaseArray (b1, 1, 10)

    if ((size(b1) /= 10) .or. (any (b1%id /= 1))) error stop 4_4

    print *, 'before last call'

    call createBaseArray (b1, arraySize=5)

    if ((size(b1) /= 5) .or. (any (b1%id /= -1))) error stop 5_4

    print *, 'end'
end
