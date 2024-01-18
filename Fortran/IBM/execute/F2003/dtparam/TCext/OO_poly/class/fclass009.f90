! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass009.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (defined operator usage in
!                               ASSOCIATE construct)
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
        integer(k1)   :: value
    end type

    interface operator (+)
        function b1AddI (b1, i)
        import base
            type (base(4)), intent(in) :: b1
            integer*4, intent(in) :: i
            type (base(4)) b1AddI
        end function

        function iAddB1 (i, b1)
        import base
            integer*4, intent(in) :: i
            type (base(4)), intent(in) :: b1
            type (base(4)) iAddB1
        end function
    end interface
end module

program fclass009
use m
    type (base(4)) :: b1

    b1%value = 10

    associate (x => (1+b1), x1 => (b1 + 100))
        print *, x%value, x
        print *, x1, x1%value

        print *, 1 + x, x+10
        print *, 10+x1, x1+1
    end associate
end

type (base(4)) function b1AddI (b1, i)
use m, only: base
    type (base(4)), intent(in) :: b1
    integer*4, intent(in) :: i

    b1AddI%value = b1%value + i
end function

type (base(4)) function iAddB1 (i, b1)
use m, only: base
    integer*4, intent(in) :: i
    type (base(4)), intent(in) :: b1

    iAddB1%value = b1%value + i
end function
