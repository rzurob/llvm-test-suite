! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_tpbnd/specific/ftpbnd519.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/01/2005
!*
!*  DESCRIPTION                : specific type bound (a type bound that returns
!                               unlimited pointer array as results; dynamic type
!                               of derived type with pointer components)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), pointer, private :: data(:) => null()

        contains

        procedure :: assgn => assgnVal
        procedure :: getVal => getBaseVal
        final :: finalizeBase
    end type

    contains

    subroutine assgnVal (b, x)
        class (base(4,*)), intent(out) :: b
        class (*), intent(in) :: x(:)

        allocate (b%data(size(x)), source=x)
    end subroutine

    subroutine finalizeBase (b)
        type (base(4,*)), intent(inout) :: b

        if (associated (b%data)) deallocate (b%data)
    end subroutine

    class(*) function getBaseVal (b)
        class (base(4,*)), intent(in) :: b
        pointer getBaseVal (:)

        getBaseVal => b%data
    end function
end module

module n
    type dataType(n2,k2)    ! (20,8)
        integer, kind     :: k2
        integer, len      :: n2
        real(k2), pointer :: r1 => null()

        contains

        procedure, nopass :: copy => deepCopy
        final :: finalizeDataArray
    end type

    contains

    type (dataType(:,8)) function deepCopy (d)
        class (dataType(*,8)), intent(in) :: d(:)
        pointer :: deepCopy(:)

        allocate (dataType(20,8) :: deepCopy(size(d)))

        do i = 1, size(d)
            if (associated (d(i)%r1)) allocate (deepCopy(i)%r1, source=d(i)%r1)
        end do
    end function

    subroutine finalizeDataArray (d)
        type (dataType(*,8)), intent(inout) :: d(:)

        do i = 1, size(d)
            if (associated (d(i)%r1)) then
                print *, 'deallocating dataType%r1'

                deallocate (d(i)%r1)
            end if
        end do
    end subroutine
end module

program ftpbnd519
use m
use n
    class (base(4,:)), allocatable :: b1(:)
    type (dataType(:,8)), allocatable :: d1(:)
    class (*), pointer :: x1(:), x2(:), x3(:)

    allocate (base(4,20) :: b1(3))
    allocate (dataType(20,8) :: d1(6))

    do i = 1, 6
        allocate (d1(i)%r1, source=i*1.0_8)
    end do

    call b1(1)%assgn ((/(j, j=1,10)/))
    call b1(2)%assgn ((/(j*2.1_8, j=2, 5)/))
    call b1(3)%assgn (d1%copy(d1))

    x1 => b1(1)%getVal()
    x2 => b1(2)%getVal()
    x3 => b1(3)%getVal()

    select type (x1)
        type is (integer)
            print *, x1
        class default
            error stop 1_4
    end select

    select type (x2)
        type is (real(8))
            write (*, '(4(f10.2))') x2
        class default
            error stop 2_4
    end select

    select type (x3)
        type is (dataType(*,8))
            do i = 1, 6
                write (*, '(f10.2)', advance='no') x3(i)%r1
            end do
        class default
            error stop 3_4
    end select

    print *, ''

    !! second test
    call b1(3)%assgn (d1%copy(d1(::2)))

    select type (x4 => b1(3)%getVal())
        type is (dataType(*,8))
            do i = 1, 3
                write (*, '(f10.2)', advance='no') x4(i)%r1
            end do
        class default
            error stop 4_4
    end select

    print *, ''
    print *, 'end'
end
