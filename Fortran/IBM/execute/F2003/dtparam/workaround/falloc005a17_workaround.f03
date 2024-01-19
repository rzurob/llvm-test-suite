! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/allocate/falloc005a17.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (defined elemental binary operation as
!                               the source-expr)
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
        integer(k1)   :: value
    end type

    interface operator (**)
        elemental type (base(20,4)) function base2Power (b, i)
            import base
            type (base(20,4)), intent(in) :: b
            integer(4), intent(in) :: i
        end function
    end interface
end module

program falloc005a17
use m
    type (base(20,4)) :: b1(5)

    type (base(:,4)), allocatable :: b2(:), b3
    class (base(:,4)), pointer :: b4(:)


    b1%value = (/1,2,3,4,5/)


    allocate (b3, source= b1(5)**(1+2))

    allocate (b2(3), source= b1(1:3)**(1*2))

    allocate (b4 (size(b1)), source=b1**2**2)

    if (b3%value /= 125) error stop 1_4

    if (any (b2%value /= (/1,4,9/))) error stop 2_4

    if (any (b4%value /= (/1, 2**4, 3**4, 4**4, 5**4/))) error stop 3_4

    deallocate (b2,b3,b4)
end


elemental type (base(20,4)) function base2Power (b, i)
use m, only : base
    type (base(20,4)), intent(in) :: b
    integer(4), intent(in) :: i

    base2Power%value = b%value ** i
end function
