!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : READ
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : This TestCase writes NAMELIST data to a file
!*                               that not of the correct format, then I/O
!*                               operations are proformed on this data.
!*
!*  TEST CONDITIONS            : 1) NAMELIST reads with invalid char, external
!*                               2) NAMELIST reads with invalid format, internal
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb014

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios            ! Test Case id under test.

      character*10 chvar1, chvar2

      character*50 unit2 ( 5 )

      character*300 errmsg

      namelist /naml1/ chvar1

      namelist /naml2/ chvar2

!
! DEFINE values
!

      unit2(1) = ' &naml2'

      unit2(2) = ' chvar2 = "ab"ab" '

      unit2(3) = ' &end'

!
! WRITE to test files
!

      write ( unit = 10, fmt = '( A )', err = 10 )' &naml1'

      write ( unit = 10, fmt = '( A )', err = 10 )' &chvar2 = "ab" '

      write ( unit = 10, fmt = '( A )', err = 10 )' &end'

      rewind 10

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! TestCase 1...
!

      case_id = case_id + 1

      read ( 10, fmt = naml1, end = 100, iostat = ios, &
     & iomsg = errmsg )

100   write(18,*) errmsg
      if ( ios <> 90 ) call zzrc ( case_id )

!
! TestCase 2...
!

      case_id = case_id + 1

      read ( unit2, fmt = naml2, end = 200, iostat = ios, &
     & iomsg = errmsg )

200   write(18,*) errmsg

      if ( ios <> 90 ) call zzrc ( case_id )

! Clean up...

      close ( 10, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end               ! End of TestCase.
