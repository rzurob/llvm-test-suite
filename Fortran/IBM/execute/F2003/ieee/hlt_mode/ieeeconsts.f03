        module constants_for_ieee

        ! Constants for test

        ! IEEE Single: real(4)
        real(4), parameter :: r1_4 = 0.0_4
        real(4), parameter :: r2_4 = 1.0_4
        real(4), parameter :: r3_4 = 1.1_4
        real(4), parameter :: zr_4 = z"00000001"

        ! IEEE Double: real(8)
        real(8), parameter :: r1_8 = 0.0_8
        real(8), parameter :: r2_8 = 1.0_8
        real(8), parameter :: r3_8 = 1.1_8
        real(8), parameter :: zr_8 = z"0000000000000001"

        ! IBM Extended: real(16)
        real(16), parameter :: r1_16 = 0.0_16
        real(16), parameter :: r2_16 = 1.0_16
        real(16), parameter :: r3_16 = 1.1_16
        real(16), parameter :: zr_16 = z"00000000000000000000000000000001"

        ! Infinities

        ! IEEE Single: real(4)
        real(4), parameter :: PINF_4 = z"7f800000"
        real(4), parameter :: NINF_4 = z"ff800000"

        ! IEEE Double: real(8)
        real(8), parameter :: PINF_8 = z"7ff0000000000000"
        real(8), parameter :: NINF_8 = z"fff0000000000000"

        ! IBM Extended: real(16)
        real(16), parameter :: PINF_16 = z"7ff00000000000000000000000000000"
        real(16), parameter :: NINF_16 = z"fff00000000000000000000000000000"

        ! Zeros

        ! IEEE Single: real(4)
        real(4), parameter :: PZERO_4 = z"00000000"
        real(4), parameter :: NZERO_4 = z"80000000"

        ! IEEE Double: real(8)
        real(8), parameter :: PZERO_8 = z"0000000000000000"
        real(8), parameter :: NZERO_8 = z"8000000000000000"

        ! IBM Extended: real(16)
        real(16), parameter :: PZERO_16 = z"00000000000000000000000000000000"
        real(16), parameter :: PZERO2_16 = z"80000000000000008000000000000000"
        ! real(16) has no negative zero

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

        ! IEEE Double: real(16)
        real(16), parameter :: PNANQ_16 = z"7fffffff7ff7ffff0000000000000000"
        real(16), parameter :: PNANS_16 = z"7ff7ffff7ff7ffff0000000000000000"
        real(16), parameter :: NNANQ_16 = z"ffff0000000000000000000000000000"
        real(16), parameter :: NNANS_16 = z"fff70000000000000000000000000000"

        ! Denormal Values
        ! P means Positive, N means Negative
        ! HD means Hugest Denormal (in magnitude)
        ! TD means Tiniest Denormal (in magnitude)
        ! _4, _8, _16 represents the Kind of the number

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