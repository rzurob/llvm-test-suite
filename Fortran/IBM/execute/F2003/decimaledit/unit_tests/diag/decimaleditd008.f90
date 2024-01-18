!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditd008.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 08, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for the DECIMAL=
!*                               specifier and decimal edit mode control
!*                               descriptors. Feature 289039.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*
!*  DESCRIPTION                : This diagnostic test, checks to make sure
!*                               non-character expressions specified in
!*                               decimal= specifier are flagged.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character :: char_arr(10)
      character(10) :: my_char, my_char_arr(2), buffer
      character :: c
      integer :: my_int
      real    :: my_real
      complex :: my_complex
      integer :: int_arr(10)
      real    :: real_arr(10)

! TESTING FOR EXTERNAL FILES:

      ! character array not valid
      open(unit=77, file='decimaleditd008.dat', decimal=char_arr)
      read(77, *, decimal=char_arr)
      write(77, *, decimal=char_arr) 3.14
      open(unit=77, file='decimaleditd008.dat', decimal=my_char_arr)
      read(77, *, decimal=my_char_arr)
      write(77, *, decimal=my_char_arr) 3.14

      ! non-character arrays not valid:
      open(unit=77, file='decimaleditd008.dat', decimal=int_arr)
      read(77, *, decimal=int_arr)
      write(77, *, decimal=int_arr) 3.14
      open(unit=77, file='decimaleditd008.dat', decimal=real_arr)
      read(77, *, decimal=real_arr)
      write(77, *, decimal=real_arr) 3.14

      ! character string valid
      open(unit=77, file='decimaleditd008.dat', decimal=my_char)
      read(77, *, decimal=my_char)
      write(77, *, decimal=my_char) 3.14

      ! character array element valid
      open(unit=77, file='decimaleditd008.dat', decimal=my_char_arr(1))
      read(77, *, decimal=my_char_arr(1))
      write(77, *, decimal=my_char_arr(1)) 3.14
      open(unit=77, file='decimaleditd008.dat', decimal=char_arr(1))
      read(77, *, decimal=char_arr(1))
      write(77, *, decimal=char_arr(1)) 3.14

      ! scalar character valid:
      open(unit=77, file='decimaleditd008.dat', decimal=c)
      read(77, *, decimal=c)
      write(77, *, decimal=c) 3.14

      ! integer not valid:
      open(unit=77, file='decimaleditd008.dat', decimal=my_int)
      read(77, *, decimal=my_int)
      write(77, *, decimal=my_int) 3.14

      ! real not valid:
      open(unit=77, file='decimaleditd008.dat', decimal=my_real)
      read(77, *, decimal=my_real)
      write(77, *, decimal=my_real) 3.14

      ! complex not valid:
      open(unit=77, file='decimaleditd008.dat', decimal=my_complex)
      read(77, *, decimal=my_complex)
      write(77, *, decimal=my_complex) 3.14

! TESTING FOR INTERNAL FILES:
      ! character array not valid
      read(buffer, *, decimal=char_arr)
      write(buffer, *, decimal=char_arr) 3.14
      read(buffer, *, decimal=my_char_arr)
      write(buffer, *, decimal=my_char_arr) 3.14

      ! non-character arrays not valid:
      read(buffer, *, decimal=int_arr)
      write(buffer, *, decimal=int_arr) 3.14
      read(buffer, *, decimal=real_arr)
      write(buffer, *, decimal=real_arr) 3.14

      ! character string valid
      read(buffer, *, decimal=my_char)
      write(buffer, *, decimal=my_char) 3.14

      ! character array element valid
      read(buffer, *, decimal=my_char_arr(1))
      write(buffer, *, decimal=my_char_arr(1)) 3.14
      read(buffer, *, decimal=char_arr(1))
      write(buffer, *, decimal=char_arr(1)) 3.14

      ! scalar character valid:
      read(buffer, *, decimal=c)
      write(buffer, *, decimal=c) 3.14

      ! integer not valid:
      read(buffer, *, decimal=my_int)
      write(buffer, *, decimal=my_int) 3.14

      ! real not valid:
      read(buffer, *, decimal=my_real)
      write(buffer, *, decimal=my_real) 3.14

      ! complex not valid:
      read(buffer, *, decimal=my_complex)
      write(buffer, *, decimal=my_complex) 3.14

      end
