!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 08, 2005
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
!*                               non-character variable specified in
!*                               decimal= specifier for INQUIRE stmt
!*                               are flagged.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character :: char_arr(10)
      character(10) :: my_char, my_char_arr(2)
      character(10), parameter :: const_buff = ''
      character :: c
      integer :: my_int
      real    :: my_real
      complex :: my_complex
      integer :: int_arr(10)
      real    :: real_arr(10)

! TESTING FOR EXTERNAL FILES ONLY:

      open(unit=77, file='decimaleditd009.dat')

      ! character constant not valid:
      inquire(77,decimal=const_buff)
      inquire(77,decimal='comma')
      inquire(77,decimal='point')

      ! character array not valid
      inquire(77,decimal=char_arr)
      inquire(77,decimal=my_char_arr)

      ! non-character arrays not valid:
      inquire(77,decimal=int_arr)
      inquire(77,decimal=real_arr)

      ! character string valid
      inquire(77,decimal=my_char)

      ! character array element valid
      inquire(77, decimal=my_char_arr(1))
      inquire(77, decimal=char_arr(1))

      ! scalar character valid:
      inquire(77, decimal=c)

      ! integer not valid:
      inquire(77, decimal=my_int)

      ! real not valid:
      inquire(77, decimal=my_real)

      ! complex not valid:
      inquire(77, decimal=my_complex)

      end
