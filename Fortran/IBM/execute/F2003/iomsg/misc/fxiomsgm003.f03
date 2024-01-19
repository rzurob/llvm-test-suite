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
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : The IOMSG option should be in effective where
!*                               it's specified. This test case  uses the option
!*                               IOMSG in a READ statement while not using it in
!*                               the following READ statement, check if IOMSG is
!*                               still in effect in the following statement.
!*
!*  TEST CONDITIONS            : 1) READ past end of formatted sequential file
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgm003

      implicit none
      integer*4 ios

      character*10 varchar

      character*300 errmsg

      character*100 unit9 ( 1 )

      open ( 9, access = 'SEQUENTIAL', form = 'FORMATTED', err = 100 )

      write ( 9, fmt = '( A10 )' ) 'xXxXxXxXxX'

      write ( unit9, fmt = '( A6 )', err = 100 ) 'xXxXxXxXxX'

      rewind 9

      errmsg="abc"

      read ( 9, fmt='(A10)',  iostat=ios ) varchar

      read ( 9, fmt='(A10)',  iostat=ios , iomsg=errmsg ) varchar

      write( 18 , * ) errmsg

      errmsg="abc"

      read(unit9,fmt='(A10)',iostat=ios ) varchar,varchar

      write( 18 , * ) errmsg

      close ( 9, status = 'DELETE' )

      stop ' '

100   call zzrc ( 100 )

      end

