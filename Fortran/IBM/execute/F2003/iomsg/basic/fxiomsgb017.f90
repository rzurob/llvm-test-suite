!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb017.f
! %VERIFY: fort.18:fxiomsgb017.vf
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
!*  TEST CASE TITLE            : READing non-integer data with integer format
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : READ
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : This test case READs data from a file that
!*                               isn't integer and reads it with integer FORMAT
!*                               commands.
!*
!*
!*  TEST CONDITIONS            : 1) READ logical var. with integer mask.
!*                               2) READ character var. with integer mask.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb017
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id, ios
 
      logical varlog
 
      character*10 varchar
 
      character*300 errmsg

!
! Initialize Return Code routine to SUCCESS...
!
 
      case_id = 0
      call zzrc ( case_id )
 
!
! Define variables with dummy constants and make READ test file.
!
 
      varlog  = .TRUE.
      varchar = '''test'''
 
      open ( 9, file = 'file1', access = &
     &  'SEQUENTIAL', form = 'FORMATTED', err = 10 )
 
      write ( 9, 20 ) varlog
 
      write ( 9, 30 ) varchar
 
      rewind 9
 
20    format ( L10 )
30    format ( A10 )
100   format ( I10 )
101   format ( I5, I5)
 
 
!
! TestCase 1...
!
 
      case_id = case_id + 1
 
      read ( 9, 100, iostat = ios, iomsg = errmsg ) varlog

      write(18,*) errmsg
 
!
! TestCase 2...
!
 
      case_id = case_id + 1
 
      read ( 9, 100, iostat = ios, iomsg = errmsg ) varchar

      write(18,*) errmsg
 
! Clean up...
 
      close ( 9, status = 'DELETE' )
 
      stop ' '
 
10    call zzrc ( case_id + 100 )
 
      end                            ! End of TestCase.
