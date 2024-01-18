!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb018.f
! %VERIFY: fort.18:fxiomsgb018.vf
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
!*  TEST CASE TITLE            : Reading extreme integer values from binary data
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
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : Variable values that are larger than the allowed
!*                               allowed are read from binary data in direct 
!*                               external file.
!*
!*
!*  TEST CONDITIONS            : 1) Max integer*4 with external file.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb018
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id, ios            ! Test Case id under test.
 
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
 
      open ( 9, access = 'DIRECT', recl = 66, form = 'FORMATTED' )
 
      write ( 9, fmt = '( A )', rec = 3 ) &
     &  '100101010101010101101010101010001'

 
!
! TestCase 1...
!
 
      case_id = case_id + 1
 
      read ( 9, fmt = '( B33 )', rec = 3, iostat = ios, &
     & iomsg = errmsg ) varint4
 
      write(18,*) errmsg
 
      if ( ios <> 98 ) call zzrc ( case_id )
 
 
! Clean up..
 
      close ( 9, status = 'DELETE' )
 
      end                            ! End of TestCase.
