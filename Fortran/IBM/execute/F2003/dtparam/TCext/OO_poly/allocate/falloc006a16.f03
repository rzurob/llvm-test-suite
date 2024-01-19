! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc006a16.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (allocate the unlimted poly
!                               allocatable array; use poly-pointer array as the
!                               source-expr; use select type to verify)
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
        integer, kind            :: k1
        integer(k1), allocatable :: id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,18)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type
end module

program falloc006a16
use m
    class (base(4)), pointer :: b1(:)
    class (*), allocatable :: x1(:)

    type (child(4,1,18)), target :: c1(4)

    !! test1
    allocate (b1(2), source=(/child(4,1,18)(1, 'test 1'), child(4,1,18)(2, 'test 2')/))

    allocate (x1(size(b1)), source=b1)

    select type (x1)
        type is (base(4))
            error stop 1_4
        type is (child(4,1,*))
            if ((.not. allocated (x1(1)%id)) .or. (.not. allocated (x1(2)%id))) &
                error stop 2_4


            if (any (x1%name /= (/'test 1', 'test 2'/))) error stop 3_4

            if ((x1(1)%id /= 1) .or. (x1(2)%id /= 2)) error stop 4_4
        class default
            error stop 5_4
    end select

    deallocate (x1, b1)

    !! test 2
    c1 = (/(child(4,1,18)(i, 'xlftest'), i=1,4)/)

    b1 => c1(::2)

    allocate (x1(size(b1)), source=b1)

    select type (x1)
        type is (child(4,1,*))
            if ((.not. allocated (x1(1)%id)).or. (.not. allocated (x1(2)%id))) &
                    error stop 10_4

            if ((x1(1)%id /= 1) .or. (x1(2)%id /= 3)) error stop 11_4

            if (any(x1%name /= 'xlftest')) error stop 12_4
        class default
            error stop 13_4
    end select
end
