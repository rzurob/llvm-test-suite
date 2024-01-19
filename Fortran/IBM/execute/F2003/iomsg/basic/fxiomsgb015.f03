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
!*  DESCRIPTION                : Invalid repetition factors were read from
!*                               internal and external files by a list-directed
!*                               read.
!*
!*  TEST CONDITIONS            : 1) Repetition specified too small, external.
!*                               2) Repetition specified too large, internal.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb015

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 varint, ios

      character*30  unit2

      character*300 errmsg

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
!  Open file to read from.
!

      open ( 9, access = 'SEQUENTIAL' )

      write ( 9, fmt = '( A10 )' ) '  0*10'

      rewind 9

      unit2 = '  999999999999999999999*10'

!
! TestCase 1...
!

      case_id = case_id + 1

      read ( 9, fmt = *, iostat = ios, iomsg=errmsg ) varint

      write(18,*) errmsg

      if ( ios <> 94 ) call zzrc ( case_id )

!
! TestCase 2...
!

      case_id = case_id + 1

      read ( unit2, fmt = *, iostat = ios, iomsg=errmsg ) varint

      write(18,*) errmsg

      if ( ios <> 96 ) call zzrc ( case_id )

!  Clean up...

      close ( 9, status = 'DELETE' )

      end                            ! End of TestCase.
