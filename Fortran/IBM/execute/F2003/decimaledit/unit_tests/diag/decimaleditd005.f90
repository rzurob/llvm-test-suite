!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 05, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for the DECIMAL=
!*                               specifier and decimal edit mode control
!*                               descriptors. Feature 289039.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : This diagnostic test, checks the values
!*                               of the iostat= specifier when something
!*                               goes wrong at runtime with regards to
!*                               the decimal= specifier.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: extfile = 77

      character(20) :: buffer, my_fmt
      character(10) :: good_decim1, good_decim2, bad_decim1, bad_decim2

      integer :: ios

      open(unit=extfile, file='decimaleditd005.txt')

      good_decim1="point"
      good_decim2="comma"
      bad_decim1="coint"
      bad_decim2="pomma"

! TESTING VALID/INVALID DECIMAL= VALUES FOR EXTERNAL FILES FIRST:

      ! test 1: decimal specifier value is valid
      write(extfile, '(f4.2)', decimal=good_decim1, iostat=ios) 3.14
      if( ios /= 0 ) then
         error stop 1
      endif

      ! test 2: decimal specifier value is valid
      write(extfile, '(f4.2)', decimal=good_decim2, iostat=ios) 3.14
      if( ios /= 0 ) then
         error stop 2
      endif

      ! test 3: decimal specifier value is invalid
      write(extfile, '(f4.2)', decimal=bad_decim1, iostat=ios) 3.14
      if( ios /= 219 ) then
         error stop 3
      endif

      ! test 4: decimal specifier value is invalid
      write(extfile, '(f4.2)', decimal=bad_decim2, iostat=ios) 3.14
      if( ios /= 219 ) then
         error stop 4
      endif

! TESTING VALID/INVALID DECIMAL= VALUES FOR INTERNAL FILES:

      ! test 5: decimal specifier value is valid
      write(buffer, '(f4.2)', decimal=good_decim1, iostat=ios) 3.14
      if( ios /= 0 ) then
         error stop 5
      endif

      ! test 6: decimal specifier value is valid
      write(buffer, '(f4.2)', decimal=good_decim2, iostat=ios) 3.14
      if( ios /= 0 ) then
         error stop 6
      endif

      ! test 7: decimal specifier value is invalid
      write(buffer, '(f4.2)', decimal=bad_decim1, iostat=ios) 3.14
      if( ios /= 220 ) then
         error stop 7
      endif

      ! test 8: decimal specifier value is invalid
      write(buffer, '(f4.2)', decimal=bad_decim2, iostat=ios) 3.14
      if( ios /= 220 ) then
         error stop 8
      endif

! TESTING IOSTAT= VALUE WHEN DECIMAL IS USED IN UNFORMATTED I/O

      close(extfile)
      my_fmt="unformatted"
      open(unit=extfile, file='decimaleditd005.dat', form=my_fmt,      &
     &     decimal=good_decim2, iostat=ios)

      ! test 9: decimal= specifier is not allowed in unformatted i/o
      if( ios /= 221 ) then
         error stop 9
      endif

      ios = -1234 ! reset ios

      open(unit=extfile, file='decimaleditd005.dat', form=my_fmt,      &
     &     decimal=good_decim1, iostat=ios)

      ! test 10: decimal= specifier is not allowed in unformatted i/o
      if( ios /= 221 ) then
         error stop 10
      endif

      close(extfile)

      end
