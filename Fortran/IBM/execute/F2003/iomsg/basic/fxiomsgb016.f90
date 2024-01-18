!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb016.f
! %VERIFY: fort.18:fxiomsgb016.vf
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
!*  TEST CASE TITLE            : Reading extreme integer values.
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
!*  DESCRIPTION                : Integer values that are larger than the allowed
!*                               are read from direct external files.  
!*
!*
!*  TEST CONDITIONS            : 1) Max integer*1 with external file.
!*                               2) Min integer*4 with external file.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb016
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id, ios            ! Test Case id under test.
 
      integer*1 varint1
 
      integer*4 varint4

      character*300 errmsg

 
!
! Initialize Return Code routine to SUCCESS...
!
      case_id = 0
      call zzrc ( case_id )
 
!
! Open files to read from.
!
 
      open ( 9, access = 'DIRECT', recl = 20, form = 'FORMATTED' )
 
      write ( 9, fmt = '( I3 )', rec = 1 ) 128
 
      write ( 9, fmt = '( A11 )', rec = 6 ) '-2147483649'
 
!
! TestCase 1...
!
 
      case_id = case_id + 1
 
      read ( 9, fmt = '( I4 )', rec = 1, iostat = ios, &
     & iomsg = errmsg ) varint1
 
      write(18,*)errmsg
     
      if ( ios <> 96 ) call zzrc ( case_id )
 
 
!
! TestCase 2...
!
 
      case_id = case_id + 1
 
      read ( 9, fmt = '( I11 )', rec = 6, iostat = ios, &
     & iomsg = errmsg ) varint4
 
      write(18,*)errmsg

      if ( ios <> 96 ) call zzrc ( case_id )

 
! Clean up..
 
      close ( 9, status = 'DELETE' )
 
      end                            ! End of TestCase.
