!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : REWIND
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                : The UNIT command has to be in the range 1
!*                               through 2147483647, and I/O statement REWIND
!*                               is only for file connected for sequential
!*                               access, not for direct access.
!*
!*  TEST CONDITIONS            : 1) I/O statements with unit number -9
!*                               2) REWIND statement with direct formatted file
!*                               3) REWIND statement with direct unfmtted file
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb010

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 a, ios

      character*10 varchar

      character*300 errmsg

!
!  Unit number too small ( unit = -9 )
!

      a = -9

!
! TestCase 1...
!

      case_id = case_id + 1

      rewind ( unit = a, err = 10, iostat = ios, iomsg = errmsg )

      call zzrc ( case_id )

 10   write(18,*) errmsg

      if ( ios <= 0 ) call zzrc ( case_id )

!
! Create file to rewind on ...
!
      varchar = 'trust'

      open ( 8, form = 'FORMATTED', access = 'DIRECT', recl = 80 )

      write ( 8, err = 40, rec = 1, fmt = '( A10 )' ) varchar

      open ( 9, access = 'DIRECT', recl = 80 )

      write ( 9, err = 40, rec = 1 ) varchar

!
! TestCase 2...
!

      case_id = case_id + 1

      rewind ( 8, err = 20, iostat =ios, iomsg = errmsg )

      call zzrc ( case_id )

 20   write(18,*) errmsg

      if ( ios <= 0 ) call zzrc ( case_id )

!
! TestCase 3...
!

      case_id = case_id + 1

      rewind ( 9, err = 30, iostat =ios, iomsg = errmsg )

      call zzrc ( case_id )

 30   write(18,*) errmsg

      if ( ios <= 0 ) call zzrc ( case_id )

! Clean up...

      close ( 8, status = 'DELETE' )

      close ( 9, status = 'DELETE' )

      stop ' '

 40   call zzrc ( case_id + 100 )

      end                     ! End of TestCase.
