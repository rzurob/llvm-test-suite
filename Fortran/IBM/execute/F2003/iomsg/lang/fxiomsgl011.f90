!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl011.f
! %VERIFY:
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
!*  TEST CASE TITLE            : OPEN with No Error conditions
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
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : Open files with No Error conditions to check
!*                               if the iomsg specifier remains unchanged. The
!*                               iomsg specifier in OPEN statement was passed
!*                               in as one element of an array of string.
!*
!*
!*  TEST CONDITIONS            : 1) OPEN formatted file with direct access
!*                               2) OPEN unformatted file with sequential access
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************


      program fxiomsgl011
 
      implicit none                     ! All variables must be Declared
 
      integer*4 case_id, ios

      character*300  a_errmsg ( 5 )
 
!
! Initialize Return Code routine to SUCCESS...
!
 
      case_id = 0
      call zzrc ( case_id )

!
! TestCase 1...
!
 
      case_id = case_id + 1

      a_errmsg (2) = 'abc'

      open ( 8, access = 'DIRECT', recl = 10, err = 10, &
   &  form = 'FORMATTED', iostat = ios, iomsg = a_errmsg(2) )
 
      if ( a_errmsg (2) <> 'abc' )  call  zzrc ( case_id )

 
!
! TestCase 2...
!
 
      case_id = case_id + 1

      a_errmsg (3) = 'abc'

      open ( 9, access = 'SEQUENTIAL', err = 10, &
     &  iostat = ios, iomsg = a_errmsg (3) )
 
      if ( a_errmsg (3) <> 'abc' )  call  zzrc ( case_id )


! Clean up...
 
      close ( 8, status = 'DELETE' )
 
      close ( 9, status = 'DELETE' )
 
      stop ' '
 
10    call zzrc ( case_id + 100 )
 
      end                            ! End of TestCase.

