!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrf018.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 25, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the VALUE
!*                               attribute when used with characters of
!*                               length other than 1. ( Feature 298120 )
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Access substrings of a character object
!*                               with VALUE attribute.
!*                               This tests characters of len > 1.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1)
           character(3), value :: c1
         end subroutine
         subroutine s2(c)
           character(87), value :: c
         end subroutine
      end interface

      character(3) :: c
      character(87) :: cc

      c = 'AaA'
      cc = "AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA"

      call s1(c)

      if( c .ne. 'AaA' ) error stop 2

      call s2(cc)

      if( cc .ne. repeat('AaA',29) ) error stop 6

      end

      subroutine s1(c1)
        character(3), value :: c1
        if ( c1(1:3) .ne. 'AaA' ) error stop 1
        c1(1:3) = 'BbB'
      end subroutine

      subroutine s2(c)
        character(87), value :: c
        if ( c(2:4) .ne. 'aAA' ) error stop 3
        if ( c(5:7) .ne. 'aAA' ) error stop 4
        c(2:4) = 'xxx'
        if ( c(10:12) .ne. 'AaA' ) error stop 5
      end subroutine
