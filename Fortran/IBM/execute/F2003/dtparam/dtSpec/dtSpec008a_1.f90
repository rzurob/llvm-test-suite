!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/02/3006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: The function return results put into
!                               select type; use the variable length for the
!                               result. Extends the test case of dtSpec008a.f
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real :: data (n)

        contains

        generic :: add => addData, appendData
        procedure :: addData => addBaseData
        procedure :: appendData => appendBaseData
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: names(n)

        contains

        procedure :: addData => addChildData
        procedure :: appendData => appendChildData
    end type

    contains

    function addBaseData (b1, b2)
        class(base(*)), intent(in) :: b1, b2
        class(base(n=max(b1%n, b2%n))), allocatable :: addBaseData

        allocate (addBaseData)

        if (b1%n >= b2%n) then
            addBaseData%data = b1%data
            addBaseData%data(:b2%n) = addBaseData%data(:b2%n) + b2%data
        else
            addBaseData%data = b2%data
            addBaseData%data(:b1%n) = addBaseData%data(:b1%n) + b1%data
        end if
    end function

    function addChildData (b1, b2)
        class(child(*, l=*)), intent(in) :: b1
        class(base(*)), intent(in) :: b2

        class(base(n=max(b1%n, b2%n))), allocatable :: addChildData

        character(b1%l) temps(max(b1%n, b2%n))

        if (b1%n >= b2%n) then
            temps = b1%names

            select type (b2)
                type is (child(n=*, l=*))
                    do i = 1, b2%n
                        temps(i) = trim(temps(i))//b2%names(i)
                    end do
            end select
        else
            temps(:b1%n) = b1%names

            select type (b2)
                type is (child(*,*))
                    do i = 1, b1%n
                        temps(i) = trim(temps(i)) // b2%names(i)
                    end do

                    temps(b1%n+1:) = b2%names(b1%n+1:)

                type is (base(*))
                    temps(b1%n+1:) = 'default'
            end select
        end if

        allocate (child(max(b1%n, b2%n), l=b1%l) :: addChildData)

!        associate (x => b1%base%add(b2))
!            addChildData%data = x%data
!        end associate
        call associate_replacer (b1%base%add(b2))

        select type (addChildData)
            type is (child(*, l=*))
                addChildData%names = temps

            class default
                stop 30
        end select

        contains

        subroutine associate_replacer(x)
            class (base(*)), intent(in) :: x
            addChildData%data = x%data
        end subroutine
    end function

    function appendBaseData (b1, b2, limit)
        class(base(*)), intent(in) :: b1, b2
        integer, intent(in) :: limit
        class(base(n=min(b1%n+b2%n,limit))), allocatable :: appendBaseData

        allocate(appendBaseData)

        if (b1%n + b2%n > limit) then
            appendBaseData%data = reshape((/b1%data, b2%data/), (/limit/))
        else
            appendBaseData%data = (/b1%data, b2%data/)
        end if
    end function

    function appendChildData (b1, b2, limit)
        class(child(*,*)), intent(in) :: b1
        class(base(*)), intent(in) :: b2
        integer, intent(in):: limit

        class(base(n=min(b1%n+b2%n,limit))), allocatable :: appendChildData

        character(len=b1%l) temps(min(b1%n+b2%n,limit))

        select type (b2)
            type is (base(*))
                temps(1) = 'default'

                temps = reshape((/b1%names, (/(temps(1), i=1, b2%n)/)/), &
                    (/min(b1%n+b2%n,limit)/))

            type is (child(*,*))
                temps = reshape((/b1%names, b2%names/), &
                    (/min(b1%n+b2%n,limit)/))
        end select

        allocate (child(n=min(b1%n+b2%n,limit), l=b1%l) :: appendChildData)

!        associate (x => b1%base%add(b2, limit))
!            appendChildData%data = x%data
!        end associate

        call associate_replacer (b1%base%add(b2, limit))
        select type (appendChildData)
            type is (child(*,*))
                appendChildData%names = temps
        end select

        contains

        subroutine associate_replacer(x)
            class (base(*)), intent(in) :: x
            appendChildData%data = x%data
        end subroutine
    end function
end module

program dtSpec008a_1
use m
    class (base(:)), allocatable :: b1, b2
    type (child(:,:)), pointer :: c1

    logical(4), external :: precision_r4

    allocate (b1, source=base(20)((/(i*1.0, i=1,20)/)))
    allocate (b2, source=child(10, 20)((/(i*1.0e1, i=1,10)/), 'xlftest'))

    allocate (child(20, 15):: c1)

    c1%data = (/(i*1.0e2, i=1, 20)/)
    c1%names = (/'01','02','03','04','05','06','07','08','09','10','11',  &
            '12','13','14','15','16','17','18','19','20'/)

    select type (x => b2%add(b1))
        type is (child(*,*))
            do i = 1, 10
                if (.not. precision_r4(x%data(i), i*1.1e1)) error stop 2_4

                if (x%names(i) /= 'xlftest') error stop 3_4
            end do

            do i = 11, 20
                if (.not. precision_r4(x%data(i), i*1.0)) error stop 12_4

                if (x%names(i) /= 'default') error stop 13_4
            end do

        class default
            error stop 1_4
    end select

    select type (x => b2%add(c1))
        class is (child(*,*))
            do i = 1, 10
                if (.not. precision_r4 (x%data(i), i*1.1e2)) error stop 5_4

                if (x%names(i) /= 'xlftest'//c1%names(i)) error stop 6_4
            end do

            do i = 11, 20
                if (.not. precision_r4 (x%data(i), i*1.0e2)) error stop 7_4

                if (x%names(i) /= c1%names(i)) error stop 8_4
            end do

        class default
            error stop 4_4
    end select

    select type (x => c1%add(b1, 25))
        type is (child(*,*))
            do i = 1, 20
                if (.not. precision_r4(x%data(i), i*1.0e2)) error stop 11_4

                if (x%names(i) /= c1%names(i)) error stop 14_4
            end do

            do i = 21, 25
                if (.not. precision_r4(x%data(i), (i-20)*1.0)) error stop 15_4

                if (x%names(i) /= 'default') error stop 16_4
            end do

        class default
            error stop 10_4
    end select

    select type (x => c1%add(b2, 35))
        class is (child(*,*))
            do i = 1, 20
                if (.not. precision_r4(x%data(i), i*1.0e2)) error stop 21_4

                if (x%names(i) /= c1%names(i)) error stop 22_4
            end do

            do i = 21, 30
                if (.not. precision_r4(x%data(i), (i-20)*1.0e1)) error stop 23_4

                if (x%names(i) /= 'xlftest') error stop 24_4
            end do

        class default
            error stop 20_4
    end select
end
