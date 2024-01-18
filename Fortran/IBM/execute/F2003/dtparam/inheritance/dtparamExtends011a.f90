!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/09/2005
!*
!*  DESCRIPTION                : miscellaneous (the equivalent program without
!                               dtparam for dtparamExtends011.f)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base4
        integer(4), private, allocatable :: k2(:)

        character(20) :: name = 'default'
        logical, private, pointer :: l2 => null()
    end type

    type base8
        integer(8), private, allocatable :: k2(:)

        character(20) :: name = 'default'
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
        type (base4), intent(in) :: b1

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
        type (base8), intent(in) :: b1

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
        type (base4) genBase4
        integer(4), intent(in) :: k2(:)
        character(*), intent(in) :: name
        logical, intent(in) :: l2

        allocate (genBase4%k2(size(k2)), source=k2)
        genBase4%name = name
        allocate (genBase4%l2, source=l2)
    end function

    type (base8) function genBase8 (k2, name, l2)
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
    type, extends(base4) :: child4

        real(8) data
        complex(4) cx(10)
    end type

    type, extends(base8) :: child8

        real(8) data
        complex(8) cx(10)
    end type

    type (child4), save :: c1_m(2)

    interface assgnVal
        module procedure assgnVal4
        module procedure assgnVal8
    end interface

    contains

    subroutine assgnVal4 (c, data, cx)
        type (child4), intent(inout) :: c
        real(8), intent(in) :: data
        complex(4), intent(in) :: cx(10)

        c%data = data
        c%cx = cx
    end subroutine

    subroutine assgnVal8 (c, data, cx)
        type (child8), intent(inout) :: c
        real(8), intent(in) :: data
        complex(8), intent(in) :: cx(10)

        c%data = data
        c%cx = cx
    end subroutine
end module

program dtparamExtends011
use m1
    type (child8) c1
    complex(4) cx(10)

    !! verify the default values
    call printBase(c1_m(2)%base4)
    call printBase(c1%base8)

    !! assign base values to c1 and c1_m
    c1%base8 = base((/1_8, 10_8/), 'c1: 8, 20, 8, 10', .true.)
    c1_m%base4 = (/base((/-1_4, -10_4, -100_4/), name='c1_m(1), 4, 20, 8, 10', &
        l2 = .true.), base(k2=(/1_4, 2_4, 2_4/), name='c1_m(1), 4, 20, 8, 10',&
        l2 = .true.)/)

    !! assign extended type's components
    call assgnVal(c1, 1.34d0, (/((i*1.0d0, i+1.0d0), i=1,10)/))

    cx = (/1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.3/)

    call assgnVal (c1_m(1), 2.21d0, cx)
    call assgnVal (c1_m(2), 5.32d0, cx+1.0)

    !! now verify the results
    call printBase(c1%base8)

100 format ("data = ", f6.2, /, "cx = ", 3("(", f6.2, ",", f6.2, "). "))

    write (*, 100) c1%data, c1%cx

    call printBase(c1_m(1)%base4)
    write (*, 100) c1_m(1)%data, c1_m(1)%cx

    call printBase(c1_m(2)%base4)
    write (*, 100) c1_m(2)%data, c1_m(2)%cx
end
