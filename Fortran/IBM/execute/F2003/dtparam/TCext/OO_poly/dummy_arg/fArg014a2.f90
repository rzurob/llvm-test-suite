! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg014a2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (assumed-size array in
!                               select type construct)
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
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1)    name
    end type
end module

program fArg014a2
use m
    class (base(4)), allocatable :: b1(:)

    allocate (b1(3), source=(/child(4,1,15)(1,'test1'), child(4,1,15)(2,'test2'), &
        child(4,1,15)(3,'test3')/))


    call abc (b1)

    call abc (b1(1:3:2))

    call abc ((/base(4)(10), base(4)(20), base(4)(30)/))

    call abc ((/child(4,1,15) (100, 'xlftest 100'), child(4,1,15)(200, 'xlftest 200')/))

    contains

    subroutine abc (b)
        class (base(4)), intent(in) :: b(*)

        select type (b)
            type is (base(4))
                print *, b(1), b(2)
            type is (child(4,1,*))
                print *, b(1), b(2)
            class default
                error stop 1_4
        end select
    end subroutine
    end
