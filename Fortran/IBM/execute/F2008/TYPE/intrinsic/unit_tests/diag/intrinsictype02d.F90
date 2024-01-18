!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : intrinsictype02d.f
!*
!* PROGRAMMER                 : David Nichols
!* DATE                       : March 10, 2011
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : Intrinsic types in TYPE spec 
!*
!* DRIVER STANZA              : xlf2008
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              Intrinsic types in TYPE spec 
!*                              with language level
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program intrinsictype02d

      TYPE (integer)    :: i1
      TYPE (integer(4)) :: i2
      TYPE (integer*4)  :: i3

      TYPE (real)    :: r1
      TYPE (real(4)) :: r2
      TYPE (real*4)  :: r3

      TYPE (complex)    :: cp1
      TYPE (complex(8)) :: cp2
      TYPE (complex*8)  :: cp3

      TYPE (character)    :: ch1
      TYPE (character(4)) :: ch2
      TYPE (character*4)  :: ch3

      TYPE (logical)    :: l1
      TYPE (logical(1)) :: l2
      TYPE (logical*1)  :: l3

      TYPE (byte)    :: b1

      TYPE (vector(unsigned))    :: vu1
      TYPE (vector(unsigned(1))) :: vu2
      TYPE (vector(unsigned*1))  :: vu3

      end
