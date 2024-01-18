! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd500a1.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd500a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (C458: functional test
!                               using external procedures as the type bound for
!                               PASS binding; only possible for types decalred
!                               in a module)
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

        contains

        procedure :: getID => getBaseID
        procedure :: setID => setBaseID
    end type

    interface
        subroutine setBaseID (b, id)
        import base
            class (base(*,4)), intent(inout) :: b
            integer(4), intent (in) :: id
        end subroutine

        integer(4) function getBaseID (b)
        import base
            class (base(*,4)), intent(in) :: b
        end function
    end interface
end module

subroutine setBaseID (b, id)
use m, only : base
    class (base(*,4)), intent(inout) :: b
    integer(4), intent (in) :: id

    b%id = id
end

integer(4) function getBaseID (b)
use m, only : base
    class (base(*,4)), intent(in) :: b

    getBaseID = b%id
end function

program ftpbnd500a1
use m
    type, extends (base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name
    end type

    class (base(:,4)), allocatable :: b1, b2(:)

    !! test b1
    allocate (b1, source=child(20,4,1)(100, 'xlftest team'))

    call b1%setID (10)

    if (b1%getID() /= 10) error stop 1_4

    select type (b1)
        type is (child(*,4,1))
            if (b1%name /= 'xlftest team') error stop 2_4
        class default
            error stop 3_4
    end select

    !! test b2
    allocate (b2(0:1), source=(/child(20,4,1)(1, 'b2 00'), child(20,4,1)(2, 'b2 01')/))

    call b2(0)%setID (100)
    call b2(1)%setID (200)

    if ((b2(0)%getID() /= 100) .or. (b2(1)%getID() /= 200)) error stop 4_4

    select type (b2)
        type is (child(*,4,1))
            if (any(b2%name /= (/'b2 00', 'b2 01'/))) error stop 5_4
        class default
            error stop 6_4
    end select
end
