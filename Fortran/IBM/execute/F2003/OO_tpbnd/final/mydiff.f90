
! this program will compare one file against two.  A success is returned if
! the first matches either one of the two files

! usage: mydiff <out-put> <compare1> <compare2>
program mydiff
    character(200) :: argv(3)

    if (command_argument_count() /= 3) error stop 100

    call get_command_argument(1, argv(1))
    call get_command_argument(2, argv(2))
    call get_command_argument(3, argv(3))

    open (1, file = argv(1), status='old', err=101)
    open (2, file = argv(2), status='old', err=102)
    open (3, file = argv(3), status='old', err=103)

    if (.not. sameContent([1,2])) then
        rewind 1
        if (.not. sameContent([1,3])) then
            stop 1
        end if
    end if

    stop

101 print *, argv(1), ' can not be opened'
    stop 101

102 print *, argv(2), ' can not be opened'
    stop 102

103 print *, argv(3), ' can not be opened'
    stop 103

    contains

    logical function sameContent (units)
        integer, intent(in) :: units(2)

        character(1000) :: line1, line2
        integer :: retcd1, retcd2

        retcd1 = 0
        retcd2 = 0

        sameContent = .true.

        do while (sameContent .and. (retcd1 == 0))
            read(units(1), '(a)', iostat=retcd1) line1
            read(units(2), '(a)', iostat=retcd2) line2

            if (is_iostat_end(retcd1)) then
                sameContent = is_iostat_end(retcd2)
            else
                sameContent = line1 == line2
            end if
        end do
    end function
end
