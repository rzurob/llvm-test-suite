! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg039.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/24/2005
!*
!*  DESCRIPTION                : dummy-arg (explicit-shape array and TARGET
!*                               attribute)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), pointer :: data(:) => null()

        contains

        final finalizeBase

        procedure :: print => printBase
    end type

    type, extends(base) :: child    ! (4,20)
        class(*), allocatable :: data2(:)

        contains

        procedure :: print => printChild
    end type

    class (base(4,:)), pointer :: b1_m(:,:) => null()

    contains

    subroutine finalizeBase (b)
        type (base(4,*)), intent(inout) :: b

        if (associated (b%data)) then
            print *, 'deallocating data'

            deallocate (b%data)
        end if
    end subroutine

    subroutine printBase (b)
        class (base(4,*)), intent(in) :: b

        if (associated(b%data)) then
            select type (x=>b%data)
                type is (character(*))
                    write (*, '(5(a,1x))') x
                type is (real)
                    write (*, '(5f10.2)') x
                type is (complex)
                    write (*, '(3("(", f10.2, ",", f10.2,")", 1x))') x
                type is (integer)
                    print *, x
            end select
        end if
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, 'child type:'
        write (*, '(a, 1x)', advance='no') 'parent part:'

        call b%base%print

        if (allocated (b%data2)) then
            select type (x=>b%data2)
                type is (character(*))
                    write (*, '(5(a,1x))') x
                type is (real)
                    write (*, '(5f10.2)') x
                type is (complex)
                    write (*, '(3("(", f10.2, ",", f10.2,")", 1x))') x
                type is (integer)
                    print *, x
            end select
        end if
    end subroutine


    subroutine printArray (b)
        class(*), target :: b(2,2)

        select type (b)
            class is (base(4,*))
                b1_m => b

                call b1_m(1,1)%print
                call b1_m(2,1)%print
                call b1_m(1,2)%print
            class default
                error stop 10_4
        end select
    end subroutine
end module


program fArg039
use m
    class(*), pointer :: x1(:,:,:)
    class(*), allocatable, target :: x2(:)

    allocate (base(4,20)::x1(2,2,2))

    select type (x1)
        class is (base(4,*))
            allocate (x1(1,1,1)%data(2), source=(/10, 20/))
            allocate (x1(2,1,1)%data(2), source=(/1.0, 3.1/))
            allocate (x1(1,2,1)%data(3), source=(/'test 01', 'test 02', 'test 03'/))
            allocate (x1(2,2,1)%data(3), source=(/(2.1, 2.2), (3.1, 3.3), (1.5, 1.6)/))
            allocate (x1(1,1,2)%data(4), source=(/100, 200, 300, 400/))
            allocate (x1(2,1,2)%data(4), source=(/'ab', 'cd', 'ef', 'gh'/))
            allocate (x1(1,2,2)%data(4), source=(/(1.5, 2.5), (3.5, 4.5), (5.5, 6.5), &
                            (7.5, 8.5)/))
        class default
            error stop 1_4
    end select

    call printArray (x1)            !<-- (1,1,1); (2,1,1); (1,2,1)

    print *, 'test 2'

    call printArray (x1(:,2,:))     !<-- (1,2,1); (2,2,1); (1,2,2)

    !! test 3

    allocate (child(4,20) :: x2(8))

    select type (x2)
        class is (child(4,*))
            select type(x1)
                class is (base(4,*))
                    x2(1)%data => x1(1,1,1)%data
                    x2(2)%data => x1(2,1,1)%data
                    x2(3)%data => x1(1,2,1)%data
                    x2(4)%data => x1(2,2,1)%data
                    x2(5)%data => x1(1,1,2)%data
                    x2(6)%data => x1(2,1,2)%data
                    x2(7)%data => x1(1,2,2)%data
                class default
                    error stop 3_4
            end select

            do i = 1, 7
                allocate (x2(i)%data2(size(x2(i)%data)), source=x2(i)%data)
            end do
        class default
            error stop 2_4
    end select

    print *, 'test 3'

    call printArray (x2(::2))   !<-- 1, 3, 5, 7

    deallocate (x2)
end
