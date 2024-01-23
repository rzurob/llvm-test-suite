        module constants_for_ieee

        ! Infinities

        ! IEEE Single: real(4)
        real(4), parameter :: PINF_4 = z"7f800000"
        real(4), parameter :: NINF_4 = z"ff800000"

        ! IEEE Double: real(8)
        real(8), parameter :: PINF_8 = z"7ff0000000000000"
        real(8), parameter :: NINF_8 = z"fff0000000000000"

        ! Zeros

        ! IEEE Single: real(4)
        real(4), parameter :: PZERO_4 = z"00000000"
        real(4), parameter :: NZERO_4 = z"80000000"

        ! IEEE Double: real(8)
        real(8), parameter :: PZERO_8 = z"0000000000000000"
        real(8), parameter :: NZERO_8 = z"8000000000000000"

        ! NaNs

        ! IEEE Single: real(4)
        real(4), parameter :: PNANQ_4 = z"7fffffff"
        real(4), parameter :: PNANS_4 = z"7fbfffff"
        real(4), parameter :: NNANQ_4 = z"ffffffff"
        real(4), parameter :: NNANS_4 = z"ffbfffff"

        ! IEEE Double: real(8)
        real(8), parameter :: PNANQ_8 = z"7fffffff7ff7ffff"
        real(8), parameter :: PNANS_8 = z"7ff7ffff7ff7ffff"
        real(8), parameter :: NNANQ_8 = z"ffff000000000000"
        real(8), parameter :: NNANS_8 = z"fff7000000000000"

        ! Denormal Values
        ! P means Positive, N means Negative
        ! HD means Hugest Denormal (in magnitude)
        ! TD means Tiniest Denormal (in magnitude)
        ! _4, _8 represents the Kind of the number

        ! IEEE Single: real(4)
        real(4), parameter :: PHD_4 = z"007fffff"
        real(4), parameter :: PTD_4 = z"00000001"
        real(4), parameter :: NHD_4 = z"807fffff"
        real(4), parameter :: NTD_4 = z"80000001"

        ! IEEE Double: real(8)
        real(8), parameter :: PHD_8 = z"000fffffffffffff"
        real(8), parameter :: PTD_8 = z"0000000000000001"
        real(8), parameter :: NHD_8 = z"800fffffffffffff"
        real(8), parameter :: NTD_8 = z"8000000000000001"

        end module
