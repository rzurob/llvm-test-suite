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
!*  NUMBER OF TESTS CONDITIONS : 6
!*
!*  DESCRIPTION                : Internal and external files are created, write,
!*                               rewind and then a read is preformed. Errors for
!*                               END of FILE are no longer issued because the
!*                               END= is coded in the read statement,but it
!*                               should still be stored in iomsg_variable.
!*
!*  TEST CONDITIONS            : 1) Read past end of file seq. formatted ext.
!*                               2) Read past end of file list directed ext.
!*                               3) Read past end of file Namelist ext.
!*                               4) Read past end of file seq. formatted int.
!*                               5) Read past end of file list directed int.
!*                               6) Read past end of file Namelist internal.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb004

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 varint

      integer*4 ios

      character*10 varchar

      character*300 errmsg

      character*100 unit9 ( 3 ), unit10 ( 3 ), unit11 ( 3 ), unit12 ( 3 )

      namelist /naml1/ varint

      namelist /naml2/ varchar

      call setrteopts("cnverr=no")

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! Create files to read from.
!

      open ( 9, access = 'SEQUENTIAL', form = 'FORMATTED', err = 10 )

      write ( 9, fmt = '( A )', err = 10 ) 'xXxXxX'

      open ( 11, access = 'SEQUENTIAL', form = 'FORMATTED', err = 10 )

      write ( 11, fmt = *, err = 10 ) 'xXxXxX'

      open ( 12, access = 'SEQUENTIAL', form = 'FORMATTED', err = 10 )

      write ( 12, fmt = naml1, err = 10 )

      unit9  ( 1 ) = 'xxxxxx'

      unit10 ( 1 ) = ''' xxxxx '''

      write ( unit11, nml = naml1, err = 10 )

!
! TestCase 1...
!

      case_id = case_id + 1

      errmsg = ' '

      read ( 9, fmt = '( A )', iostat = ios, iomsg=errmsg,end = 100 )

      call zzrc ( case_id )

100   write(18, *) errmsg
      if ( ios >= 0 ) call zzrc (case_id )

!
! TestCase 2...
!

      case_id = case_id + 1

      errmsg = ' '

      read ( 11, fmt = *, iostat = ios,iomsg=errmsg, end = 300 ) varchar

      call zzrc ( case_id )

300   write(18, *) errmsg
      if ( ios >= 0 ) call zzrc (case_id )

!

!
! TestCase 3...
!

      case_id = case_id + 1

      errmsg = ' '

      read ( 12, nml = naml1, iostat = ios,iomsg=errmsg,  end = 400 )
      if ( ios >= 0 ) call zzrc (case_id )

      call zzrc ( case_id )

400   write(18, *) errmsg

!

!
! TestCase 4...
!

      case_id = case_id + 1

      errmsg = ' '

      read (unit9,fmt='(A)',iostat=ios,iomsg=errmsg, end=500)  &
   &  varchar, varchar, varchar, varchar, varchar

      call zzrc ( case_id )

500   write(18, *) errmsg
      if ( ios >= 0 ) call zzrc (case_id )

!
! TestCase 5...
!

      case_id = case_id + 1

      errmsg = ' '

      read (unit10,fmt=*,iostat=ios,iomsg=errmsg, end=600)  &
   &  varchar, varchar, varchar,varchar

      call zzrc ( case_id )

600   write(18, *) errmsg
      if ( ios >= 0 ) call zzrc (case_id )

!

!
! TestCase 6...
!

      case_id = case_id + 1

      errmsg = ' '

      read ( unit11, nml = naml2, iostat = ios,iomsg=errmsg, end = 700 )

      call zzrc ( case_id )

700   write(18, *) errmsg
      if ( ios >= 0 ) call zzrc (case_id )

!  Clean up..
      close ( 9, status = 'DELETE' )

      close ( 11, status = 'DELETE' )

      close ( 12, status = 'DELETE' )

      stop ' '

 10   call zzrc ( case_id + 100 )

      end                            ! End of TestCase.
