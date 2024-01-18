!* =================================================================== &
!*
!* DATE                       : March 10, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Intrinsic types in TYPE spec
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              Intrinsic types in CLASS spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program intrinsictype01d

      CLASS (integer)    :: i1
      CLASS (integer(4)) :: i2
      CLASS (integer*4)  :: i3

      CLASS (real)    :: r1
      CLASS (real(4)) :: r2
      CLASS (real*4)  :: r3

      CLASS (complex)    :: cp1
      CLASS (complex(8)) :: cp2
      CLASS (complex*8)  :: cp3

      CLASS (character)    :: ch1
      CLASS (character(4)) :: ch2
      CLASS (character*4)  :: ch3

      CLASS (logical)    :: l1
      CLASS (logical(1)) :: l2
      CLASS (logical*1)  :: l3

      CLASS (byte)    :: b1

      CLASS (vector(unsigned)) :: v1
      CLASS (vector(byte))     :: v2
      CLASS (vector(pixel))    :: v3

      end

