! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet001a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : function result (poly-function return results;
!                               use select type construct to verify the data
!                               type)
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
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    interface
        class(*) function producePtrOfAnyType (x)
            pointer producePtrOfAnyType
            class (*), intent(in) :: x
        end function
    end interface

end module

program ffuncRet001a1
use m

    class (*), allocatable :: x0

    allocate (x0, source=child(8,1,15)(20, 'test2'))

    select type (x => producePtrOfAnyType (100_8))
        type is (integer(8))
            if (x /= 100) error stop 10_4
        class default
            error stop 1_4
    end select

    select type (x => producePtrOfAnyType (child(8,1,15)(10, 'test1')))
        class is (base(8))
            select type (y => x)
                type is (child(8,1,*))
                    if ((y%id /= 10) .or. (y%name /= 'test1')) error stop 11_4
                class default
                    error stop 3_4
            end select
        class default
            error stop 2_4
    end select

    select type (x => producePtrOfAnyType (x0))
        type is (base(8))
            error stop 4_4
        type is (child(8,1,*))
            if ((x%id /= 20) .or. (x%name /= 'test2')) error stop 12_4
        class default
            error stop 5_4
    end select
end


class (*) function producePtrOfAnyType (x)
    pointer producePtrOfAnyType
    class (*), intent(in) :: x

    allocate (producePtrOfAnyType, source=x)
end function
