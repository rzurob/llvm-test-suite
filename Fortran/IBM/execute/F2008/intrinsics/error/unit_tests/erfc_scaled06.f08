     ! Test SP/DP/QP erfc_scaled for exceptional inputs.

      implicit none
      real(4) :: r4, r4_res
      real(8) :: r8, r8_res
      real(16) :: r16, r16_res
      logical precision_r16

      ! erfc_scaled(+Inf) == 0
      r4 = 1.0_4/0.0_4
      r8 = 1.0_8/0.0_8
      r16 = 1.0_16/0.0_16
      r4_res = erfc_scaled(r4)
      r8_res = erfc_scaled(r8)
      r16_res = erfc_scaled(r16)
      if (r4_res .ne. 0.0) error stop 1
      if (r8_res .ne. 0.0) error stop 2
      if (.NOT. precision_r16(r16_res, 0.0_16)) error stop 3

      ! erfc_scaled(-Inf) == +Inf
      r4 = -1.0_4/0.0_4
      r8 = -1.0_8/0.0_8
      r16 = -1.0_16/0.0_16
      r4_res = erfc_scaled(r4)
      r8_res = erfc_scaled(r8)
      r16_res = erfc_scaled(r16)
      if (r4_res .ne. 1.0_4/0.0_4) error stop 4
      if (r8_res .ne. 1.0_8/0.0_8) error stop 5
      if (r16_res .ne. 1.0_16/0.0_16) error stop 6

      ! erfc_scaled(0) == 1
      r4 = 0.0_4
      r8 = 0.0_8
      r16 = 0.0_16
      r4_res = erfc_scaled(r4)
      r8_res = erfc_scaled(r8)
      r16_res = erfc_scaled(r16)
      if (r4_res .ne. 1.0_4) error stop 7
      if (r8_res .ne. 1.0_8) error stop 8
      if (r16_res .ne. 1.0_16) error stop 9

      ! erfc_scaled(NaN) is NaN
      r4 = 0.0_4/0.0_4
      r8 = 0.0_8/0.0_8
      r16 = 0.0_16/0.0_16
      r4_res = erfc_scaled(r4)
      r8_res = erfc_scaled(r8)
      r16_res = erfc_scaled(r16)
      if (r4_res .eq. r4_res) error stop 10
      if (r8_res .eq. r8_res) error stop 11
      if (r16_res .eq. r16_res) error stop 12

      end
