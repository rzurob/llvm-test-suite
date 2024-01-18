!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use of substring's substring in a recursive
!                               subroutine in call to inquire statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

recursive subroutine inquireSubStr (unit, s, i)
    character(*), intent(inout) :: s
    integer, intent(in) :: i, unit

    if (i <= 0) stop 100

    if (i == 1) then
        inquire (unit, decimal=s)
    else
        call inquireSubStr (unit, s(i:), i-1)
    end if
end subroutine


program dcmlCharVarInquir002a
    character(:), allocatable :: val(:)

    integer shiftVal(10)

    allocate (character(100) :: val(10))

    val(:)(:) = ''

    open (8, file='test1')

    shiftVal(1) = 10

    !! POINT mode
    call inquireSubStr (8, val(1), shiftVal(1))

    open (8, decimal='COMMA')

    shiftVal(2) = 3

    !! COMMA mode
    call inquireSubStr (8, val(2), shiftVal(2))

    open (8, access='stream', iostat=istat)

    if (istat == 0) error stop 1_4

    shiftVal(3) = 4

    !! still COMMA mode
    call inquireSubStr (8, val(3), shiftVal(3))

    shiftVal(4) = 12

    !! UNDEFINED
    call inquireSubStr (9, val(4), shiftVal(4))

    open (8, delim='QUOTE', form='formatted', err=100, pad='no', round='UP', &
        sign='plus', status='old')

    shiftVal(5) = 5

    !! stays in COMMA mode
    call inquireSubStr (8, val(5), shiftVal(5))


    !! 9 to be connected to test1 is NOT allowed
    open (9, delim='QUOTE', form='formatted', pad='no', round='UP', &
        sign='plus', status='old', file='test1', iostat=istat)

    if (istat == 0) error stop 2_4

    shiftVal(6:7) = 7

    !! UNDEFINED mode
    call inquireSubStr (9, val(6), shiftVal(6))

    !! unit 8 stays in COMMA mode
    call inquireSubStr (8, val(7), shiftVal(7))

    !! old connection is closed, new file mode is POINT
    open (8, file='test')

    !! this is OK, in POINT mode
    open (9, delim='QUOTE', form='formatted', pad='no', round='UP', &
        sign='plus', status='old', file='test1')

    shiftVal(8) = 1
    shiftVal(9) = 9

    !! POINT mode
    call inquireSubStr (8, val(8), shiftVal(8))

    !! POINT mode
    call inquireSubStr (9, val(9), shiftVal(9))


    !! verify val
    if (val(1) /= genBlanks(shiftVal(1))//'POINT    ') error stop 10_4
    if (val(2) /= genBlanks(shiftVal(2))//'COMMA') error stop 11_4
    if (val(3) /= genBlanks(shiftVal(3))//'COMMA') error stop 12_4
    if (val(4) /= genBlanks(shiftVal(4))//'UNDEFINED') error stop 13_4
    if (val(5) /= genBlanks(shiftVal(5))//'COMMA') error stop 14_4
    if (val(6) /= genBlanks(shiftVal(6))//'UNDEFINED') error stop 15_4
    if (val(7) /= genBlanks(shiftVal(7))//'COMMA') error stop 16_4
    if (val(8) /= genBlanks(shiftVal(8))//'POINT') error stop 17_4
    if (val(9) /= genBlanks(shiftVal(9))//'POINT') error stop 18_4


    stop
100 stop 10

    contains

    function genBlanks (i)
        integer, intent(in) :: i

        character(i*(i-1)/2) genBlanks

        genBlanks = ''
    end function
end

