!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb024.f
! %VERIFY: fort.18:fxiomsgb024.vf
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
!*  TEST CASE TITLE            : FORMAT has invalid width spec. in READ.
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
!*  NUMBER OF TESTS CONDITIONS : 5
!*
!*  DESCRIPTION                : Test FORMAT specifiers that have an invalid 
!*                               width spec. with the READ statement.
!*
!*
!*  TEST CONDITIONS            : 1) Invalid field width with I format code.
!*                               2) Invalid field width with F format code.
!*                               3) Invalid field width with L format code.
!*                               4) Invalid field width with A format code.
!*                               5) Invalid field width with B format code.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb024
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id                 ! Test Case id under test.
 
      integer*4 varint, ios
 
      character*20 form

      character*300 errmsg
 
!
! Initialize Return Code routine to SUCCESS...
!
 
      case_id = 0
      call zzrc ( case_id )
 
!
!  OPEN file to read from
!
 
      open ( 9, access = 'SEQUENTIAL', err = 10 )
 
      write ( 9, fmt = '( A5 )', err = 10 ) 'XXXXX'
 
      write ( 9, fmt = '( A5 )', err = 10 ) 'XXXXX'
 
      rewind 9
 
 
!
! TestCase 1...
!
 
      case_id = case_id + 1
 
      form = '( I0 )'
 
      read ( 9, fmt = form, iostat = ios, iomsg=errmsg, end = 10 ) varint

      write(18, *) errmsg
 
      form = '( I2001 )'
 
      read ( 9, fmt = form, iostat = ios, iomsg=errmsg, end = 10 ) varint

      write(18, *) errmsg
 
      rewind 9
 
!
! TestCase 2...
!
 
      case_id = case_id + 1
 
      form = '( F0.1 )'
 
      read ( 9, fmt = form, iostat = ios, iomsg=errmsg, end = 10 ) varint
 
      write(18, *) errmsg
 
      form = '( F2001.1 )'
 
      read ( 9, fmt = form, iostat = ios, iomsg=errmsg, end = 10 ) varint
 
      write(18, *) errmsg
 
      rewind 9
 
!
! TestCase 3...
!
 
      case_id = case_id + 1
 
      form = '( L0 )'
 
      read ( 9, fmt = form, iostat = ios, iomsg=errmsg, end = 10 ) varint
 
      write(18, *) errmsg
 
      form = '( L2001 )'
 
      read ( 9, fmt = form, iostat = ios, iomsg=errmsg, end = 10 ) varint
 
      write(18, *) errmsg
 
      rewind 9
 
!
! TestCase 4...
!
 
      case_id = case_id + 1
 
      form = '( A0 )'
 
      read ( 9, fmt = form, iostat = ios, iomsg=errmsg, end = 10 ) varint
 
      write(18, *) errmsg
 
      form = '( A32768 )'
 
      read ( 9, fmt = form, iostat = ios, iomsg=errmsg, end = 10 ) varint
 
      write(18, *) errmsg
 
      rewind 9
 
 
!
! TestCase 5...
!
 
      case_id = case_id + 1
 
      form = '( B0.1 )'
 
      read ( 9, fmt = form, iostat = ios, iomsg=errmsg, end = 10 ) varint
 
      write(18, *) errmsg
 
      form = '( B2001.1 )'
 
      read ( 9, fmt = form, iostat = ios, iomsg=errmsg, end = 10 ) varint
 
      write(18, *) errmsg
 
      rewind 9
 
 
! Clean up...
 
      close ( 9, status = 'DELETE' )
 
      stop ' '
 
10    call zzrc ( case_id + 100 )
 
      end                            ! End of TestCase.
