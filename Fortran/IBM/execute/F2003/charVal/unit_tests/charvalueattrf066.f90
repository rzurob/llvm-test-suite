!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 01, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the VALUE
!*                               attribute when used with characters of
!*                               length other than 1. ( Feature 298120 )
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test when argument is a concatination of
!*                               two or more other character strings.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main


      character(3) :: a
      character(1) :: b
      character(2) :: c, d

      a = 'AbC'
      b = 'd'
      c = 'Ab'
      d = 'Ef'

      call s1(a//b)

      if( a .ne. 'AbC' .or. b .ne. 'd' ) error stop 2

      call s1(c//'C'//b)

      if( c .ne. 'Ab' .or. b .ne. 'd' ) error stop 3

      call s1(a//b//c)

      call s1(a//b//c//'12321341')


      call s2(a//b, d)

      if( a .ne. 'AbC' .or. b .ne. 'd' ) error stop 33

      call s2(c//'C'//b, 'Ef')

      if( c .ne. 'Ab' .or. b .ne. 'd' ) error stop 44

      call s2(a//b//c, d)

      if ( a .ne. 'AbC' .or. b .ne. 'd' .or. c .ne. 'Ab' .or. d .ne. 'Ef' ) error stop 55

      call s2('AbC'//'d'//'e', d)

      if ( d .ne. 'Ef' ) error stop 66

      call s2(a//b//c//'12321341', 'Ef')

      contains
      subroutine s1(x)
        character(4), value :: x
        if ( x .ne. 'AbCd' ) error stop 1
      end subroutine
      subroutine s2(x, y)
        character(4), value :: x
        character(2), value :: y
        if ( x .ne. 'AbCd' ) error stop 11
        if ( y .ne. 'Ef' ) then
           error stop 22
        endif
      end subroutine

end program

