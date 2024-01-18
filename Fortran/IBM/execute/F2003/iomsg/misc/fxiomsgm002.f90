!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgm002.f
! %VERIFY: fort.18:fxiomsgm002.vf
! %STDIN: 
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
!***************************************************************************


!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            : I/O error message will be truncated.
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : WRITE
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : When an I/O error happened, the corresponding
!*                               error message with be put in iomsg specifier. 
!*                               If the size of iomsg specifier is less than 
!*                               the actual error message, then the error 
!*                               message should be truncated to fit the size of
!*                               iomsg specifier.
!*
!*
!*  TEST CONDITIONS            : 1) Write direct formatted file with rec = 0
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************


      program fxiomsgm002

      implicit none


      integer*4 case_id

      integer*4 ios

      integer*4 rec_num

      character*10 varchar

      character*50 errmsg


!
! Initialize Return Code routine to 0
!

      case_id = 0
      call zzrc(case_id)


!
! Create file
!

      rec_num = 0

      open ( 10 , access = 'DIRECT', recl = 6, form = 'FORMATTED' )


!
! TestCase 1...
!

      case_id = case_id + 1


      write ( 10, fmt = '( A )', iostat = ios, iomsg = errmsg, &
  &   rec = rec_num ) 'xXxXxX'
 
      write ( 18, * ) errmsg

      if ( LEN ( errmsg ) <> 50 )  call zzrc ( case_id )


! Clean up....

      close ( 10, status = 'DELETE' )


      end

