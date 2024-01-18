!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : DOT_PRODUCT intrinsic
!*
!* DESCRIPTION                : real and complex types
!* ===================================================================

implicit none

logical :: precision_r4, precision_r8, precision_r16
logical :: precision_x8, precision_x16, precision_x32

real(4) :: r4=dot_product((/1.0, 2.0/), (/2.0, 3.0/))
real(8) :: r8=dot_product((/1.0_8, 2.0_8/), (/2.0_8, 3.0_8/))
real(16) :: r16=dot_product((/1.0_16, 2.0_16/), (/2.1_16, 3.9_16/))

complex(4) :: c4=dot_product((/ (3.0,1.0), (3.0,1.0) /), &
                           & (/ (1.0,2.0), (1.0,2.0) /))
complex(8) :: c8=dot_product((/ (3.7_8,1.0_8), (3.3_8,1.0_8) /), &
                           & (/ (1.0_8,2.1_8), (1.0_8,2.3_8) /))
complex(16) :: c16=dot_product((/ (3.711_16,1.9_16), (3.37_16,1.0_16) /), &
                             & (/ (1.022_16,2.1_16), (1.01_16,2.3_16) /))

if (.not. precision_r4(r4, dot_product((/1.0, 2.0/), (/2.0, 3.0/)))) stop 1
if (.not. precision_r8(r8, dot_product((/1.0_8, 2.0_8/), (/2.0_8, &
  & 3.0_8/)))) stop 2
if (.not. precision_r16(r16, dot_product((/1.0_16, 2.0_16/), &
  & (/2.1_16, 3.9_16/)))) stop 3

if (.not. precision_x8(c4, dot_product((/ (3.0,1.0), (3.0,1.0) /), &
  & (/ (1.0,2.0), (1.0,2.0) /)))) stop 4

if (.not. precision_x16(c8, dot_product((/ (3.7_8,1.0_8), (3.3_8,1.0_8) /), &
  & (/ (1.0_8,2.1_8), (1.0_8,2.3_8) /)))) stop 5

if (.not. precision_x32(c16, dot_product((/ (3.711_16,1.9_16), &
  & (3.37_16,1.0_16) /), (/ (1.022_16,2.1_16), (1.01_16,2.3_16) /)))) stop 6

end
