! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Structure constructor with component-spec
!                               is not available if it has inaccessible
!                               components. Solution 2: use of generic name.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        private

        real(k) :: data(n)
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        private

        integer(k) :: id
        character(l) :: name
    end type

    interface base
        module procedure genBase4
        module procedure genBase8
    end interface

    interface child
        module procedure genChild4
        module procedure genChild8
    end interface

    type(base(4, 10)) :: b1

    contains

    function genBase4 (i, r1)
        integer, intent(in) :: i
        real(4), intent(in) :: r1(i)

        type(base(4,i)) genBase4

        genBase4%data = r1
    end function

    function genBase8 (i, d1)
        integer, intent(in) :: i
        real(8), intent(in) :: d1(i)

        type(base(8,i)) genBase8

        genBase8%data = d1
    end function

    function genChild4 (i, r1, len, id, name)
        integer, intent(in) :: i, len
        real(4), intent(in) :: r1(i)
        integer(4), intent(in) :: id
        character(*), intent(in) :: name

        type(child(4, i, len)) genChild4

        genChild4%base = base(i, r1)
        genChild4%id = id
        genChild4%name = name
    end function

    function genChild8 (i, r1, len, id, name)
        integer, intent(in) :: i, len
        real(8), intent(in) :: r1(i)
        integer(8), intent(in) :: id
        character(*), intent(in) :: name

        type(child(8, i, len)) genChild8

        genChild8%base = base(i, r1)
        genChild8%id = id
        genChild8%name = name
    end function

    subroutine verifyBase4 (b, r1)
        class(base(4,10)), intent(in) :: b
        real(4), intent(in) :: r1(10)

        logical(4), external :: precision_r4

        do i = 1, 10
            if (.not. precision_r4(b%data(i), r1(i))) error stop 1_4
        end do
    end subroutine

    subroutine verifyChild8 (c, d1, id, name)
        type(child(8, 433, 20)), intent(in) :: c
        real(8), intent(in) :: d1(433)
        integer(8), intent(in) :: id
        character(*), intent(in) :: name

        logical(4), external :: precision_r8

        do i = 1, 433
            if (.not. precision_r8(c%data(i), d1(i))) error stop 2_4
        end do

        if (c%id /= id) error stop 3_4
        if (c%name /= name) error stop 4_4
    end subroutine
end module

program dtparamConstr018a
use m
    type (child(8, 433, 20)) :: c1

    double precision d1(1000)

    d1 = log(1.2d0*(/(i, i=1, 1000)/))

    b1 = base (10, (/(i*1.2, i=1,10)/))

    c1 = child (433, d1, 20, (2_8**15)**3, 'a temporary name for c1: xlftest')

    !! verify
    call verifyBase4 (b1, (/(i*1.2, i=1,10)/))

    call verifyChild8 (c1, d1, 2_8**45, 'a temporary name for')
end
