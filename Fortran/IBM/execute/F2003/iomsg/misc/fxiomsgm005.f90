!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgm005.f
! %VERIFY: fort.18:fxiomsgm005.vf
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
!*  TEST CASE TITLE            : FILE= is needed in the OPEN with langlvl=90std
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : OPEN
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : Different language level has different I/O
!*                               statement support. The FILE=specifier is 
!*                               needed on langlvl '90std' while not necessary
!*                               on langlvl 'extended'. This test case checks
!*                               if the iomsg is catching this error message.
!*
!*  TEST CONDITIONS            : 1) OPEN with FILE= missing on langlvl 90std
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgm005
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id                 ! Test Case id under test.
      integer*4 ios
      character*400 errmsg

call setrteopts("langlvl=90std")

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! TestCase 1...
!
 
      case_id = case_id + 1

      open ( 18, file = 'fort.18' )
      open ( 8, err = 10, iostat = ios, iomsg = errmsg )

      call zzrc ( case_id )

10    write ( 18, * ) errmsg
   
      if ( ios <> 151 ) call zzrc ( case_id )

! Clean up...

      close ( 8, status = 'DELETE' )

      end                            ! End of TestCase.

