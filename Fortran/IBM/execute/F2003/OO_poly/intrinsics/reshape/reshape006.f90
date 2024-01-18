! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is poly
!*    PAD and ORDER are specified. PAD has different declared type but
!*      same dynamic type as SOURCE
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
        integer :: i = 88
    end type

    type, extends(Base) :: Child
        integer :: j = 99
    end type
end module

program reshape006
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

    type(Child) :: c1(10)
    class(Base), pointer :: b1(:,:) => null()
    class(Base), allocatable :: b2(:)

    c1 = (/ (Child(i,i+1), i=1,10) /)
    allocate(Child::b2(2))

    allocate(b1(3,5))
    b1 = reshape(c1, (/3,5/), b2, (/2,1/))

    print *, c1
    print *, reshape(c1, (/3,5/), b2, (/2,1/))
    print *, b1%i
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
