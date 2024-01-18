!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/09/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: Base type with private componets' names
!                               that are the same as the the type parameter
!                               names in the extended type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k1, l1)
        integer, kind :: k1
        integer, len :: l1 = 20

        integer(k1), private, allocatable :: k2(:)

        character(l1) :: name = 'default'
        logical, private, pointer :: l2 => null()
    end type

    interface base
        module procedure genBase4
        module procedure genBase8
    end interface

    interface printBase
        module procedure printBase4
        module procedure printBase8
    end interface

    contains

    subroutine printBase4 (b1)
        type (base(4)), intent(in) :: b1

        if (allocated(b1%k2)) then
            write(*, '(10i12)', advance='no') b1%k2
            write(*, '(1x, a)', advance='no') b1%name
        else
            write(*, '(a,a)', advance='no') 'unallocated; ', b1%name
        end if

        if (associated(b1%l2)) then
            write (*, '(l5)') b1%l2
        else
            write (*, '(a)') ' null'
        end if
    end subroutine

    subroutine printBase8 (b1)
        type (base(8)), intent(in) :: b1

        if (allocated(b1%k2)) then
            write(*, '(10i12)', advance='no') b1%k2
            write(*, '(1x, a)', advance='no') b1%name
        else
            write(*, '(a,a)', advance='no') 'unallocated; ', b1%name
        end if

        if (associated(b1%l2)) then
            write (*, '(l5)') b1%l2
        else
            write (*, '(a)') ' null'
        end if
    end subroutine

    function genBase4 (k2, name, l2)
        type (base(4)) genBase4
        integer(4), intent(in) :: k2(:)
        character(*), intent(in) :: name
        logical, intent(in) :: l2

        allocate (genBase4%k2(size(k2)), source=k2)
        genBase4%name = name
        allocate (genBase4%l2, source=l2)
    end function

    type (base(8)) function genBase8 (k2, name, l2)
        integer(8), intent(in) :: k2(:)
        character(*), intent(in) :: name
        logical, intent(in) :: l2

        allocate (genBase8%k2(size(k2)), source=k2)
        genBase8%name = name
        allocate (genBase8%l2, source=l2)
    end function
end module

module m1
use m
    type, extends(base) :: child (k2, l2)
        integer, kind :: k2
        integer, len :: l2

        real(k2) data
        complex(k1) cx(l2)
    end type

    type (child(4, 20, 8, 10)), save :: c1_m(2)

    interface assgnVal
        module procedure assgnVal4
        module procedure assgnVal8
    end interface

    contains

    subroutine assgnVal4 (c, data, cx)
        type (child(4, 20, 8, 10)), intent(inout) :: c
        real(8), intent(in) :: data
        complex(4), intent(in) :: cx(10)

        c%data = data
        c%cx = cx
    end subroutine

    subroutine assgnVal8 (c, data, cx)
        type (child(8, 20, 8, 10)), intent(inout) :: c
        real(8), intent(in) :: data
        complex(8), intent(in) :: cx(10)

        c%data = data
        c%cx = cx
    end subroutine
end module

program dtparamExtends011
use m1
    type (child(8, 20, 8, 10)) c1
    complex(4) cx(10)

    !! verify the default values
    call printBase(c1_m(2)%base)
    call printBase(c1%base)

    !! assign base values to c1 and c1_m
    c1%base = base((/1_8, 10_8/), 'c1: 8, 20, 8, 10', .true.)
    c1_m%base = (/base((/-1_4, -10_4, -100_4/), name='c1_m(1), 4, 20, 8, 10', &
        l2 = .true.), base(k2=(/1_4, 2_4, 2_4/), name='c1_m(1), 4, 20, 8, 10',&
        l2 = .true.)/)

    !! assign extended type's components
    call assgnVal(c1, 1.34d0, (/((i*1.0d0, i+1.0d0), i=1,10)/))

    cx = (/1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.3/)

    call assgnVal (c1_m(1), 2.21d0, cx)
    call assgnVal (c1_m(2), 5.32d0, cx+1.0)

    !! now verify the results
    call printBase(c1%base)

100 format ("data = ", f6.2, /, "cx = ", 3("(", f6.2, ",", f6.2, "). "))

    write (*, 100) c1%data, c1%cx

    call printBase(c1_m(1)%base)
    write (*, 100) c1_m(1)%data, c1_m(1)%cx

    call printBase(c1_m(2)%base)
    write (*, 100) c1_m(2)%data, c1_m(2)%cx
end
