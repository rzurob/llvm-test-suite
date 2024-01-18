! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_tpbnd/specific/ftpbnd524a.f
! opt variations: -qnok -ql

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2005
!*
!*  DESCRIPTION                : specific type bound (use of the unlimited poly
!                               pointer as function return)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        private
        class(*), pointer :: data

        contains

        procedure :: getData => returnData
        procedure :: setData => setBaseData
    end type

    contains

    function returnData (b) result (r1)
        class (base(4)), intent(in) :: b

        class(*), pointer :: r1

        r1 => b%data
    end function

    elemental subroutine setBaseData (b, x)
        class (base(4)), intent(out) :: b
        class (*), intent(in) :: x

        allocate (b%data, source=x)
    end subroutine
end module

program ftpbnd524a
use m
    type T1(k2)    ! (8)
        integer, kind :: k2
        real(k2)         r1, r2
    end type

    class (base(4)), allocatable :: b1(:)

    allocate (b1(10))

    call b1(1:3)%setData ((/1,2,3/))

    call b1(4::3)%setData ((/'test 04', 'test 07', 'test 10'/))

    call b1(5:8:3)%setData ((/3.5_4, 2.2_4/))

    call b1(6)%setData (T1(8)(1.2_8, 3.4_8))
    call b1(9)%setData ((1.1, 2.2))

    do i = 1, 10
        select type (x => b1(i)%getData())
            class default
                error stop 1_4
            type is (integer)
                print *, x
            type is (complex(4))
                write (*, '(2f10.2)') x
            type is (character(*))
                print *, x
            type is (real(4))
                write (*, '(f8.2)') x
            type is (T1(8))
                write (*, '(f10.2, a, f10.2)') x%r1, ';', x%r2
        end select
    end do
end
