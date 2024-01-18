! GB DTP extension using:
! ftcx_dtp -qck -ql /tstdev/OO_poly/allocate/falloc006a14.f
! opt variations: -qnock -qnol

! SCCS ID Information
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
!*
!*  DATE                       : 12/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n2)    ! (20,4,1,18)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2)    name
    end type

    interface makeData
        module procedure createBase, createChild
    end interface

    contains

    elemental type (base(20,4)) function createBase (id)
        integer, intent(in) :: id

        createBase%id = id
    end function

    elemental type (child(20,4,1,18)) function createChild (id, name)
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
        type is (child(*,4,1,*))
            if (any(x1%id /= reshape(ids, (/4/)))) error stop 1_4

            if (any (x1%name /= reshape(names, (/4/)))) error stop 2_4
        class default
            error stop 4_4
    end select
end
