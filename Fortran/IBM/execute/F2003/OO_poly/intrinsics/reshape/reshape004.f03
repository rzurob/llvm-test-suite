! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is poly
!*    PAD and ORDER are not specified
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program reshape004
use m

    interface assignment (=)
        elemental subroutine tt(c, t)
            use m
            type(Child), intent(out) :: c
            type(Child), intent(in) :: t
        end subroutine

        elemental subroutine ct(c, t)
            use m
            type(Base), intent(out) :: c
            type(Child), intent(in) :: t
        end subroutine
    end interface

    type(Child) :: c1(20)
    class(Base), pointer :: b2(:,:) => null()

    c1 = (/ (Child(i,i+1), i=1,20) /)

    allocate(b2(3,5))
    b2 = reshape(c1, (/3,5/))

    print *, c1
    print *, b2%i
end

elemental subroutine tt(c, t)
    use m
    type(Child), intent(out) :: c
    type(Child), intent(in) :: t
    c%i = t%i
    c%j = t%j
end subroutine

elemental subroutine ct(c, t)
    use m
    type(Base), intent(out) :: c
    type(Child), intent(in) :: t
    c%i = t%i
end subroutine