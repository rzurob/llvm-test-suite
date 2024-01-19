        module constants_for_ieee

        ! Normal values

        ! IEEE Single: real(4)
        real(4), parameter :: PNORMAL1_4 = 2.0_4
        real(4), parameter :: PNORMAL2_4 = 4.0_4
        real(4), parameter :: NNORMAL1_4 = -2.0_4
        real(4), parameter :: NNORMAL2_4 = -4.0_4

        ! IEEE Double: real(8)
        real(8), parameter :: PNORMAL1_8 = 2.0_8
        real(8), parameter :: PNORMAL2_8 = 4.0_8
        real(8), parameter :: NNORMAL1_8 = -2.0_8
        real(8), parameter :: NNORMAL2_8 = -4.0_8

        ! IBM Extended: real(16)
        real(16), parameter :: PNORMAL1_16 = 2.0_16
        real(16), parameter :: PNORMAL2_16 = 4.0_16
        real(16), parameter :: NNORMAL1_16 = -2.0_16
        real(16), parameter :: NNORMAL2_16 = -4.0_16

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
        real(4), parameter :: pnanq_lowest_4 = z'7FC00000'
        real(4), parameter :: pnanq_highest_4 = z'7FFFFFFF'
        real(4), parameter :: nnanq_lowest_4 = z'FFC00000'
        real(4), parameter :: nnanq_highest_4 = z'FFFFFFFF'
        real(4), parameter :: pnans_lowest_4 = z'7F800001'
        real(4), parameter :: pnans_highest_4 = z'7FBFFFFF'
        real(4), parameter :: nnans_lowest_4 = z'FF800001'
        real(4), parameter :: nnans_highest_4 = z'FFBFFFFF'

        ! IEEE Double: real(8)
        real(8), parameter :: PNANQ_8 = z"7fffffff7ff7ffff"
        real(8), parameter :: PNANS_8 = z"7ff7ffff7ff7ffff"
        real(8), parameter :: NNANQ_8 = z"ffff000000000000"
        real(8), parameter :: NNANS_8 = z"fff7000000000000"
        real(8), parameter :: pnanq_lowest_8 = z'7FF8000000000000'
        real(8), parameter :: pnanq_highest_8 = z'7FFFFFFFFFFFFFFF'
        real(8), parameter :: nnanq_lowest_8 = z'FFF8000000000000'
        real(8), parameter :: nnanq_highest_8 = z'FFFFFFFFFFFFFFFF'
        real(8), parameter :: pnans_lowest_8 = z'7FF0000000000001'
        real(8), parameter :: pnans_highest_8 = z'7FF7FFFFFFFFFFFF'
        real(8), parameter :: nnans_lowest_8 = z'FFF0000000000001'
        real(8), parameter :: nnans_highest_8 = z'FFF7FFFFFFFFFFFF'

        ! IEEE Double: real(16)
        real(16), parameter :: PNANQ_16 = z"7fffffff7ff7ffff0000000000000000"
        real(16), parameter :: PNANS_16 = z"7ff7ffff7ff7ffff0000000000000000"
        real(16), parameter :: NNANQ_16 = z"ffff0000000000000000000000000000"
        real(16), parameter :: NNANS_16 = z"fff70000000000000000000000000000"
        real(16), parameter :: pnanq_lowest_16 = z'7FF80000000000000000000000000000'
        real(16), parameter :: pnanq_highest_16 = z'7FFFFFFFFFFFFFFF0000000000000000'
        real(16), parameter :: nnanq_lowest_16 = z'FFF80000000000000000000000000000'
        real(16), parameter :: nnanq_highest_16 = z'FFFFFFFFFFFFFFFF0000000000000000'
        real(16), parameter :: pnans_lowest_16 = z'7FF00000000000010000000000000000'
        real(16), parameter :: pnans_highest_16 = z'7FF7FFFFFFFFFFFF0000000000000000'
        real(16), parameter :: nnans_lowest_16 = z'FFF00000000000010000000000000000'
        real(16), parameter :: nnans_highest_16 = z'FFF7FFFFFFFFFFFF0000000000000000'

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
