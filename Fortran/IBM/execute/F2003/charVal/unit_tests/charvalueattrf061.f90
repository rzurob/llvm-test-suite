!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrf061.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : charvalueattrf061
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Jan. 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the VALUE
!*                               attribute when used with characters of 
!*                               length other than 1. ( Feature 298120 )   
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Test that pass-by-value using VALUE attribute
!*                               works as expected for characters of length 87.
!*                               This tests procedures with more than 1 argument.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1, c2, c3, c4)
           character*87, value :: c1, c2, c4
           character(87) c3
         end subroutine
         subroutine s2(c1, c2, c3, c4)
           character(87) :: c1, c2, c4
           character(87), value :: c3
         end subroutine
      end interface
      
      character(87) :: x1, x2, x3, x4
      x1 = 'AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA'
      x2 = 'BbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbB'
      x3 = 'CcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcC'
      x4 = 'DdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdD'

      call s1('AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA', &
     &         'BbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbB',&
     &         'CcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcC',&
     &         'DdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdD')

      call s1(x1, x2, x3, x4)
      if( x1 .ne. repeat('AaA',29) ) error stop 9
      if( x2 .ne. repeat('BbB',29) ) error stop 10
      if( x3 .ne. repeat('vVv',29) ) error stop 11
      if( x4 .ne. repeat('DdD',29) ) error stop 12
      
      call s1(x1, repeat('BbB',29), &
     &         'CcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcC', x4)
      if( x1 .ne. repeat('AaA',29) ) error stop 13
      if( x4 .ne. repeat('DdD',29) ) error stop 14

      x1 = 'AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA'
      x2 = 'BbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbB'
      x3 = 'CcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcC'
      x4 = 'DdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdD'

      call s2('AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA', &
     &        'BbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbB', &
     &         'CcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcC',&
     &         'DdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdD')


      call s2(x1, x2, x3, x4)
      if( x1 .ne. repeat('tTt',29) ) error stop 15
      if( x2 .ne. repeat('uUu',29) ) error stop 16
      if( x3 .ne. repeat('CcC',29) ) error stop 17
      if( x4 .ne. repeat('wWw',29) ) error stop 18
      
      x1 = repeat('AaA',29)
      x4 = repeat('DdD',29)
      call s2(x1, 'BbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbB', &
     &        'CcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcC', x4)
      if( x1 .ne. repeat('tTt',29) ) error stop 19
      if( x4 .ne. repeat('wWw',29) ) error stop 20
      
      x3 = 'CcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcCCcC'
      call s2('AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA', &
     &        'BbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbBBbB', &
     &         x3, &
     &         'DdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdDDdD')
      if( x3 .ne. repeat('CcC',29) ) error stop 21
      
      end

         subroutine s1(c1, c2, c3, c4)
           character*87, value :: c1, c2, c4
           character(87) c3
           if ( c1 .ne. repeat('AaA',29) ) error stop 1
           if ( c2 .ne. repeat('BbB',29) ) error stop 2
           if ( c3 .ne. repeat('CcC',29) ) error stop 3
           if ( c4 .ne. repeat('DdD',29) ) error stop 4
           c1 = repeat('tTt',29)
           c2 = repeat('uUu',29)
           c3 = repeat('vVv',29)
           c4 = repeat('wWw',29)
         end subroutine

         subroutine s2(c1, c2, c3, c4)
           character(87) :: c1, c2, c4
           character(87), value :: c3
           if ( c1 .ne. repeat('AaA',29) ) error stop 5
           if ( c2 .ne. repeat('BbB',29) ) error stop 6
           if ( c3 .ne. repeat('CcC',29) ) error stop 7
           if ( c4 .ne. repeat('DdD',29) ) error stop 8
           c1 = repeat('tTt',29)
           c2 = repeat('uUu',29)
           c3 = repeat('vVv',29)
           c4 = repeat('wWw',29)
         end subroutine
