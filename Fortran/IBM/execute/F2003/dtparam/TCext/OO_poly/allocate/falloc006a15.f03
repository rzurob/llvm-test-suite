! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc006a15.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (allocate unlimited poly pointer array
!                               using a function call that returns a poly
!                               allocatable array)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(base) :: child(k2,k3,n2)    ! (4,20,4,1,18)
        integer, kind             :: k2,k3
        integer, len              :: n2
        integer(k2)                  id
        character(kind=k3,len=n2)    name
    end type

    contains

    class (base(4,20)) function produceBaseAllocArray (b1)
        class (base(4,*)), intent(in) :: b1(:)

        allocatable produceBaseAllocArray(:)

        allocate (produceBaseAllocArray(size(b1)), source=b1)
    end function
end module

program falloc006a15
use m
    class (*), pointer :: x1(:)

    allocate (x1(3), source=produceBaseAllocArray((/child(4,20,4,1,18)(1,'test1'), &
                child(4,20,4,1,18)(2,'test2'), child(4,20,4,1,18)(3, 'test3')/)))

    select type (y => x1(::2))
        type is (child(4,*,4,1,*))
            if (any(y%id /= (/1, 3/))) error stop 1_4

            if (any(y%name /= (/'test1', 'test3'/))) error stop 2_4
        class default
            error stop 3_4
    end select
end
