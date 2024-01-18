
module m
    type base (k1, l1)
        integer(1), kind :: k1
        integer(k1), len :: l1

        real(k1) data(l1)
    end type

    type, extends(base) :: child(k2, l2)
        integer(k1), kind :: k2
        integer(k2), len :: l2

        integer(k2) id
        character(l2) name
    end type

    type (child(8, 18, k2 = 4, l2 = 20)) c1_m
end module

program dtparamExtends004
use m
    logical(4) precision_r4, precision_r8
    type (child(4, 2, 8, 30)) c1


    !! assign the values to the components of c1_m and c1
    c1_m%data = (/(i*1.1d0, i = 1, c1_m%l1)/)
    c1_m%id = 10
    c1_m%name = 'xlftest c1_m'

    c1%data = (/(i*10.3_4, i=1, c1%l1)/)
    c1%id = 20
    c1%name = 'xlftest c1'

    !! verify the type parameters and data component values
    if ((kind(c1_m%id) /= 4) .or. (kind(c1%id) /= 8)) error stop 1_4
    if ((len(c1_m%name) /= 20) .or. (len(c1%name) /= 30)) error stop 2_4

    if ((c1_m%id /= 10) .or. (c1%id /= 20)) error stop 3_4
    if ((c1_m%name /= 'xlftest c1_m') .or. (c1%name /= 'xlftest c1')) &
                error stop 4_4

    do i = 1, 18
        if (.not. precision_r8(c1_m%data(i), i*1.1d0)) error stop 5_4
    end do

    if (.not. precision_r4 (c1%data(1), 10.3_4)) error stop 6_4
    if (.not. precision_r4 (c1%data(2), 20.6_4)) error stop 7_4

end
