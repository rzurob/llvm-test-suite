!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb022.f
! %VERIFY: fort.18:fxiomsgb022.vf
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
!*  TEST CASE TITLE            : NAMELIST with invalid inputs, int. and ext.
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
!*  NUMBER OF TESTS CONDITIONS : 4
!*
!*  DESCRIPTION                : This TestCase reads data from internal and
!*                               external files with additional namelist names.
!* 
!*
!*  TEST CONDITIONS            : 1) NAMELIST reads with extra var name internal.
!*                               2) NAMELIST reads with extra ary name internal.
!*                               3) NAMELIST reads with extra var name external.
!*                               4) NAMELIST reads with extra ary name external.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb022
 
      implicit none                     ! All variables must be Declared
 
      integer*4 varint, aryint ( 2 )
 
      character*10 varchar, arychar ( 2 )
 
      character*50 unit1 ( 5 ), unit2 ( 5 )
 
      namelist /naml1/ varint, varchar

      namelist /naml2/ aryint, arychar
 
!
! DEFINE values
!
 
      varint       = 10
      aryint ( 1 ) = 11
      aryint ( 2 ) = 12
 
      varchar       = 'XXXXX'
      arychar ( 1 ) = 'AAAAA'
      arychar ( 2 ) = 'ZZZZZ'
 
!
! WRITE to test files
!
 
      write ( unit1, fmt = naml1, err = 10 )
 
      write ( unit2, fmt = naml2, err = 10 )
 
      write ( 9, fmt = naml1, err = 10 )
 
      write ( 10, fmt = naml2, err = 10 )
 
      rewind 9
 
      rewind 10
 
      call bus ( unit1, unit2 )
 
      stop ' '
 
10    call zzrc ( 100 )
 
      end                   ! End of main.
 
      subroutine bus ( unit1, unit2 )
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id                 ! Test Case id under test.
 
      integer*4 varint, aryint ( 2 ), ios
 
      character*50 unit1 ( 5 ), unit2 ( 5 )
 
      character*300 errmsg
 
      namelist /naml1/ varint

      namelist /naml2/ aryint
 
!
! Initialize Return Code routine to SUCCESS...
!
 
      case_id = 0
      call zzrc ( case_id )
 
 
!
! TestCase 1...
!
 
      case_id = case_id + 1
 
      read ( unit1, fmt = naml1, end = 100, iostat = ios,  &
     & iomsg = errmsg )
 
100   write(18,*) errmsg

      if ( ios <> 88 ) call zzrc ( case_id )

 
!
! TestCase 2...
!
 
      case_id = case_id + 1
 
      read ( unit2, fmt = naml2, end = 200, iostat = ios,  &
     & iomsg = errmsg )
 
200   write(18,*) errmsg
      if ( ios <> 88 ) call zzrc ( case_id )
 

!
! TestCase 3...
!
 
      case_id = case_id + 1
 
      read ( 9, fmt = naml1, end = 300, iostat = ios,  &
     & iomsg = errmsg )
 

300   write(18,*) errmsg

      if ( ios <> 88 ) call zzrc ( case_id )
 
      rewind 9
 
!
! TestCase 4...
!
 
      case_id = case_id + 1
 
      read ( 10, fmt = naml2, end = 400, iostat = ios,  &
     & iomsg = errmsg )
 

400   write(18,*) errmsg

      if ( ios <> 88 ) call zzrc ( case_id )
 
! Clean up...
 
      close ( 9, status = 'DELETE' )
 
      close ( 10, status = 'DELETE' )
 
      return
 
      end               ! End of sub and TestCase.
