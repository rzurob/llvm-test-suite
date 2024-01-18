!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb028.f
! %VERIFY: fort.18:fxiomsgb028.vf
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
!*  TEST CASE TITLE            : I/O with blank/undefined format command.
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : READ WRITE
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : The READ and WRITE format commands must not 
!*                               contain just a blank, also can't be an 
!*                               undefined format code.
!*
!*
!*  TEST CONDITIONS            : 1) WRITE with just a blank in format code.
!*                               2) READ with undefined format code.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb028
 
      implicit none 
 
 
      integer*4 case_id, ios     

      integer*4 testvar
 
      character*1 fmt1

      character   fmt2

      character*300 errmsg

 
!
! Initialize Return Code routine to SUCCESS...
!
      case_id = 0
      call zzrc ( case_id )
 
 
      fmt1 = ' '
 
      testvar = 90
 
      open ( 9, file = 'tmpfile.vf', err = 10 )
 
 
!
! TestCase 1...
!
 
      case_id = case_id + 1
 
      write ( 9, fmt = fmt1, iostat = ios, iomsg = errmsg ) testvar

      write(18, *) errmsg
 
      rewind 9
 
!
! TestCase 2...
!
 
      case_id = case_id + 1
 
      read ( 9, fmt = fmt2, iostat = ios, iomsg = errmsg ) testvar

      write(18, *) errmsg
 
 
! Clean up...
 
      close ( 9, status = 'DELETE' )
 
      stop ' '
 
10    call zzrc ( case_id + 100 )
 
      end                            ! End of TestCase.
