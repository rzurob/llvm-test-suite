
program dcmlCharVarInquir003

    character(20) :: s1
    character(:), pointer :: s2

    open (1, file='test', decimal='comma')

    s1 = 'initially defined'

    allocate (s2, source='')

    s2 = 'IBM'

    inquire (unit=1, decimal=s2)

    call acquireDecMode (s1(10:1), 1)

    if (s1 /= 'initially defined') error stop 1_4

    if (s2%len /= 0) error stop 2_4

    contains

    subroutine acquireDecMode (s, unit)
        character(*), intent(out) :: s
        integer, intent(in) :: unit

        inquire (unit, decimal=s)
    end subroutine
end
