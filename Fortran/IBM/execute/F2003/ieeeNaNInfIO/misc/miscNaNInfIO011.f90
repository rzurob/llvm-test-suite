!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : miscNaNInfIO011.f
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
!*  Testing default I/O with objects of derived type containing
!*  REAL and COMPLEX components.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      module m

        implicit none

        type :: base
          character(3) :: name
          real(kind=4) :: real_arr(3) = ( (/0.0, 0.0, 0.0/) )
        end type

        type, extends(base) :: ext_t
          complex(kind=4) :: cx_value = (0.0, 0.0)
          contains
            procedure, pass :: reset
        end type

        contains

        ! Resets the components of type ext_t
        subroutine reset(arg)
          class(ext_t) :: arg

          arg%name = 'xxx'
          arg%real_arr = (/ -1.0, -1.0, -1.0 /)
          arg%cx_value = (-1.0, -1.0)
        end subroutine

      end module

program miscNaNInfIO011

      use m
      implicit none

      type(ext_t) :: obj(2)

      integer, parameter :: in = 11, out = 12

      open(in,  file='miscNaNInfIO011.dat', action='read')
      open(out, file='miscNaNInfIO011.out', action='write')

      call obj(1)%reset
      call obj(2)%reset

      print *, obj

      read(in, *) obj

      ! *******************************************
      ! check for the signs of NaN values read.
      ! *******************************************
      if ( .not. equiv_is_positive( obj(1)%real_arr(1) ) ) then
         error stop 1_4
      end if
      if ( .not. equiv_is_positive( obj(1)%real_arr(2) ) ) then
         error stop 2_4
      end if
      if ( .not. equiv_is_positive( real( obj(1)%cx_value ) ) ) then
         error stop 3_4
      end if
      if ( .not. equiv_is_positive( imag( obj(1)%cx_value ) ) ) then
         error stop 4_4
      end if
      if ( .not. equiv_is_negative( obj(2)%real_arr(2) ) ) then
         error stop 5_4
      end if
      if ( .not. equiv_is_negative( imag( obj(2)%cx_value ) ) ) then
         error stop 6_4
      end if

      ! *******************************************
      ! write the values out for verification
      ! *******************************************
      write(out, *, sign='plus') obj(:)

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      call obj(1)%reset
      call obj(2)%reset

      read(in, *, decimal='comma') obj

      ! *******************************************
      ! check for the signs of NaN values read.
      ! *******************************************
      if ( .not. equiv_is_negative( obj(1)%real_arr(1) ) ) then
         error stop 7_4
      end if
      if ( .not. equiv_is_positive( obj(1)%real_arr(2) ) ) then
         error stop 8_4
      end if
      if ( .not. equiv_is_negative( real(obj(1)%cx_value) ) ) then
         error stop 9_4
      end if
      if ( .not. equiv_is_positive( imag(obj(1)%cx_value) ) ) then
         error stop 10_4
      end if
      if ( .not. equiv_is_negative( obj(2)%real_arr(2) ) ) then
         error stop 11_4
      end if
      if ( .not. equiv_is_positive( real(obj(2)%cx_value) ) ) then
         error stop 12_4
      end if


      ! *******************************************
      ! write the values out for verification
      ! *******************************************
      write(out, *) obj(1)%name, obj(1)%real_arr(1:2),                 &
     &              obj(1)%real_arr(3), obj(1)%cx_value, obj(2)

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      call obj(1)%reset
      call obj(2)%reset

      read(in, *) obj(1)%name, obj(1)%real_arr, obj(2)%cx_value,       &
     &            obj(1)%cx_value, obj(2)%name, obj(2)%real_arr

      ! *******************************************
      ! check for the signs of NaN values read.
      ! *******************************************
      if ( .not. equiv_is_positive( obj(1)%real_arr(1) ) ) then
         error stop 13_4
      end if
      if ( .not. equiv_is_negative( obj(1)%real_arr(2) ) ) then
         error stop 14_4
      end if
      if ( .not. equiv_is_negative( real(obj(2)%cx_value) ) ) then
         error stop 15_4
      end if
      if ( .not. equiv_is_positive( imag(obj(2)%cx_value) ) ) then
         error stop 16_4
      end if
      if ( .not. equiv_is_positive( imag(obj(1)%cx_value) ) ) then
         error stop 17_4
      end if
      if ( .not. equiv_is_negative( obj(2)%real_arr(2) ) ) then
         error stop 18_4
      end if


      ! *******************************************
      ! write the values out for verification
      ! *******************************************
      write(out, *) obj


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

end program
