! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/dummy_arg/fArg005a2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child(4,1,*)), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine createChild (c, id, name)
        type (child(4,1,*)), allocatable, intent(out) :: c
        integer*4, optional, intent(in) :: id
        character(*), optional, intent(in) :: name

        allocate (c)

        if (present(id)) c%id = id
        if (present (name)) c%name = name
    end subroutine

    subroutine createBaseArray (b, id, arraySize)
        type (base(4)), allocatable, intent(out) :: b (:)
        integer*4, intent(in) :: arraySize
        integer*4, intent(in), optional :: id

        allocate (b(arraySize))

        if (present(id))  b%id = id
    end subroutine
end module

program fArg005a2
use m
    type (child(4,1,20)), allocatable :: c1
    type (base(4)), allocatable :: b1(:)

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
