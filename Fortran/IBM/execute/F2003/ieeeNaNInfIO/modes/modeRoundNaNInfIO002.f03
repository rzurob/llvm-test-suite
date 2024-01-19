!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 4, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Testing the input and output of NaN and Inf with various ROUND modes
!*  for list-directed I/O
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11, out = 12

      character(24), parameter :: fname_in = 'modeRoundNaNInfIO002.dat'
      character(24), parameter :: fname_out = 'modeRoundNaNInfIO002.out'

      real(4)    :: rl1, rl2, real_part, imag_part
      complex(4) :: cx

      integer(4) :: count = 0

      character(20), parameter :: modes(6) =  (/'                  UP',&
     &                                          '                DOWN',&
     &                                          '                ZERO',&
     &                                          '             NEAREST',&
     &                                          '          COMPATIBLE',&
     &                                          '   PROCESSOR_DEFINED'/)

      open(in,  file=fname_in, action='read')
      open(out, file=fname_out, action='write', sign='plus')


      do count=1, 6

         rl1 = 0.0; rl2 = 0.0; cx = (0.0, 0.0)

         read(in, *, round=modes(7-count)) rl1, rl2, cx

         real_part = real(cx)
         imag_part = imag(cx)

         ! rl1 should be +NaN(Q)
         if ( ( .not. ieee_is_nan(rl1) ) .or.                          &
     &        ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.             &
     &        ( .not. equiv_is_positive(rl1) ) ) call zzrc(count+100_4)

         ! rl2 should be -Inf
         if ( ieee_is_finite(rl2) .or. .not. ieee_is_negative(rl2) )   &
     &        call zzrc(count+200_4)

         ! real part of cx should be -NaN(S)
         if ( ( .not. ieee_is_nan(real_part) ) .or.                    &
     &        ( ieee_class(real_part) .ne. ieee_signaling_nan ) .or.   &
     &        ( .not. equiv_is_negative(real_part) ) )                 &
     &        call zzrc(count+300_4)

         ! imaginary part of cx should be +Inf
         if ( ieee_is_finite(imag_part) .or.                           &
     &        ieee_is_negative(imag_part) )                            &
     &        call zzrc(count+400_4)

         write(out, *, round=modes(count)) rl1, rl2, cx

      end do

      close(in)
      close(out)

      contains

      ! Returns true if the integer equivalence of
      ! the copy of the dummy argument is negative
      logical function equiv_is_negative(val)

         real(4)    :: val, tmp_val
         integer(4) :: val_eq

         equivalence(tmp_val, val_eq)

         tmp_val = val

         if ( val_eq .ge. 0 ) then
            equiv_is_negative = .false.
         else
            equiv_is_negative = .true.
         end if

      end function

      ! Returns true if the integer equivalence of
      ! the copy of the dummy argument is positive
      logical function equiv_is_positive(val)

         real(4)    :: val, tmp_val
         integer(4) :: val_eq

         equivalence(tmp_val, val_eq)

         tmp_val = val

         if ( val_eq .le. 0 ) then
            equiv_is_positive = .false.
         else
            equiv_is_positive = .true.
         end if

      end function


      end
