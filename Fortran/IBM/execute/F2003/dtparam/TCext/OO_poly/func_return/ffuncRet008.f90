! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet008.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/25/2005
!*
!*  DESCRIPTION                : poly function result (use of the RESULT
!                               keyword)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure, non_overridable :: makePtr => makeBasePtr
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    function makeBasePtr(b) result (ptr)
        class (base(4)), intent(in) :: b
        class (base(4)), pointer :: ptr

        allocate (ptr, source=b)
    end function
end module

program ffuncRet008
use m
    class (base(4)), pointer :: p1

    type (child(4,1,20)) c1

    c1 = child(4,1,20)(1, 'test')

    p1 => c1%makePtr()

    select type (p1)
        type is (child(4,1,*))
            if (p1%id /= 1) error stop 1_4

            if (p1%name /= 'test') error stop 2_4
        class default
            error stop 5_4
    end select

    deallocate (p1)
end
