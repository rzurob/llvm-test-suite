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
! %GROUP: falloc006a14.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
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
!*  DATE                       : 12/17/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : allocate (source-expr is a call to elemental
!                               function in a call to reshape(); use derived
!                               types; rank-two array converted to rank-one
!                               array)
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
        integer id
    end type

    type, extends(base) :: child
        character*18 name
    end type

    interface makeData
        module procedure createBase, createChild
    end interface

    contains

    elemental type (base) function createBase (id)
        integer, intent(in) :: id

        createBase%id = id
    end function

    elemental type (child) function createChild (id, name)
        integer, intent(in) :: id
        character(*), intent(in) :: name

        createChild%id = id
        createChild%name = name
    end function
end module

program falloc006a14
use m
    class(*), allocatable :: x1(:)

    integer ids (2,2)
    character(18) names(2,2)

    ids = reshape ((/1,2,3,4/), (/2,2/))
    names = reshape ((/'test 1', 'test 2', 'test 3', 'test 4'/), (/2,2/))

    allocate (x1(size(ids)), source=reshape(makeData (ids, names), (/4/)))

    select type (x1)
        type is (child)
            if (any(x1%id /= reshape(ids, (/4/)))) error stop 1_4

            if (any (x1%name /= reshape(names, (/4/)))) error stop 2_4
        class default
            error stop 4_4
    end select
end
