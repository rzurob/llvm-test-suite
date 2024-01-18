!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : miscNaNInfIO010.f
!*
!*  DATE                       : July 11, 2006
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
!*  Testing the Input/Output of arrays of REALs & COMPLEX that contain
!*  IEEE NaN and Inf values. Testing namelist I/O in this testcase.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11, out = 12

      integer :: i

      real(kind=4) :: real_part, imag_part

      real(kind=4), dimension(6)   :: real_arr1
      real(kind=4), dimension(3,2) :: real_arr2

      complex(kind=4), dimension(2)   :: cx_arr1
      complex(kind=4), dimension(2,2) :: cx_arr2

      namelist /naninf/ real_arr1, real_arr2, cx_arr1, cx_arr2

      open(in,  file='miscNaNInfIO010.dat', action='read')
      open(out, file='miscNaNInfIO010.out', action='write')

      ! **************************************************
      ! read from the input file and store into the arrays
      ! **************************************************

      ! reset the arrays
      call reset_arrays(real_arr1, real_arr2, cx_arr1, cx_arr2)

      read(in, nml=naninf, decimal='comma')

      ! **************************************************
      ! check the signs for NaN values just read
      ! **************************************************
      print *, real_arr1
      if ( .not. equiv_is_positive( real_arr1(1) ) ) error stop 1_4
      if ( .not. equiv_is_negative( real_arr1(4) ) ) error stop 2_4
      if ( .not. equiv_is_positive( real_arr1(6) ) ) error stop 3_4

      if ( .not. equiv_is_positive( real_arr2(2,1) ) ) error stop 4_4
      if ( .not. equiv_is_positive( real_arr2(3,1) ) ) error stop 5_4
      if ( .not. equiv_is_negative( real_arr2(2,2) ) ) error stop 6_4
      if ( .not. equiv_is_positive( real_arr2(3,2) ) ) error stop 7_4

      real_part = real(cx_arr1(1))
      imag_part = imag(cx_arr1(1))
      if ( .not. equiv_is_positive( real_part ) ) error stop 8_4
      if ( .not. equiv_is_negative( imag_part ) ) error stop 9_4

      imag_part = imag(cx_arr2(1,1))
      if ( .not. equiv_is_positive( imag_part ) ) error stop 10_4

      real_part = real(cx_arr2(2,1))
      imag_part = imag(cx_arr2(2,1))
      if ( .not. equiv_is_positive( real_part ) ) error stop 11_4
      if ( .not. equiv_is_positive( imag_part ) ) error stop 12_4

      real_part = real(cx_arr2(2,2))
      if ( .not. equiv_is_negative( real_part ) ) error stop 13_4


      ! **************************************************
      ! write the arrays to the output file
      ! **************************************************

      write(out, nml=naninf, sign='plus')

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

      ! Resets the values of all elements of the input to -1.0
      subroutine reset_arrays(rl_arr1, rl_arr2, cx_arr1, cx_arr2)

         real(kind=4), dimension(6)   :: rl_arr1
         real(kind=4), dimension(3,2) :: rl_arr2

         complex(kind=4), dimension(2)   :: cx_arr1
         complex(kind=4), dimension(2,2) :: cx_arr2

         intent(out) :: rl_arr1, rl_arr2, cx_arr1, cx_arr2

        ! reset the arrays of reals
        real_arr1 = (/ (-1.0, i=1, 6) /)
        real_arr2 = reshape( real_arr1, (/3,2/) )

        ! reset the arrays of complex
        cx_arr1 = (/ (-1.0,-1.0), (-1.0, -1.0) /)
        cx_arr2 = reshape( (/ (-1.0,-1.0), (-1.0, -1.0),               &
     &                        (-1.0,-1.0), (-1.0, -1.0) /), (/2,2/) )


      end subroutine

      end
