!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrf070.f
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
!*  TEST CASE TITLE            : charvalueattrf070
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Feb. 01, 2006
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
!*  DESCRIPTION                : test for when the proc takes more than 
!*                               1 arg and the actual args are longer than the dummy
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main

      character(10) :: c
      character(3) :: cc
      character(87) :: ccc, xx
      c = 'AbCdAbCdAb'
      cc = 'Ef'
      ccc = repeat('AbC', 29)
      xx = repeat('Ejk', 29)
!     s1
      call s1(c)
      
      if ( c .ne. 'AbCdAbCdAb' ) error stop 2
      
      call s1('AbCdAbCdAb')
      
!     s2
      call s2(c, cc)
      
      if ( c .ne. 'AbCdAbCdAb' .or. cc .ne. 'Ef' ) error stop 33
      
      call s2('AbCdAbCdAb',cc)
      
      if ( cc .ne. 'Ef' ) error stop 44
      
      call s2('AbCdAbCdAb', 'Ef')
      
      call s2(c, 'Ef')
      
      if ( c .ne. 'AbCdAbCdAb' ) error stop 55
      

!     s3
      call s3(c, cc)
      
      if ( c .ne. 'AbCdAbCdAb' .or. cc .ne. 'Ef' ) error stop 77
      
      call s3('AbCdAbCdAb',cc)
      
      if ( cc .ne. 'Ef' ) error stop 88
      
      call s3('AbCdAbCdAb', 'Ef')
      
      call s3(c, 'Ef')
      
      if ( c .ne. 'AbCdAbCdAb' ) error stop 99
      
      call s3(ccc, cc)
      
      if ( ccc .ne. repeat('AbC',29) .or. cc .ne. 'Ef' ) error stop 111

!     s4:
      call s4(c, cc)
      
      if ( c .ne. 'AbCdAbCdAb' .or. cc .ne. 'Ef' ) error stop 122
      
      call s4('AbCdAbCdAb',cc)
      
      if ( cc .ne. 'Ef' ) error stop 133
      
      call s4('AbCdAbCdAb', 'Ef')
      
      call s4(c, 'Ef')
      
      if ( c .ne. 'AbCdAbCdAb' ) error stop 144
      
      call s4(ccc, cc)
      
      if ( ccc .ne. repeat('AbC',29) .or. cc .ne. 'Ef' ) error stop 155

      
!     s5:
      
      call s5('AbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbC',cc)
      
      if ( cc .ne. 'Ef' ) error stop 166
      
      call s5('AbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbC', 'Ef')
      
      call s5(ccc, 'Ef')
      
      if ( ccc .ne. repeat('AbC',29) ) error stop 177
      
      call s5(ccc, cc)
      
      if ( ccc .ne. repeat('AbC',29) .or. cc .ne. 'Ef' ) error stop 188

!     s6:
      
      call s6('AbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbC',cc)
      
      if ( cc .ne. 'Ef' ) error stop 199
      
      call s6('AbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbC', 'Ef')
      
      call s6(ccc, 'Ef')
      
      if ( ccc .ne. repeat('AbC',29) ) error stop 211
      
      call s6(ccc, cc)
      
      if ( ccc .ne. repeat('AbC',29) .or. cc .ne. 'Ef' ) error stop 222


!     s7:
      
      call s7(cc,'AbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbC')
      
      if ( cc .ne. 'Ef' ) error stop 233
      
      call s7('Ef','AbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbC')
      
      call s7('Ef',ccc)
      
      if ( ccc .ne. repeat('AbC',29) ) error stop 244
      
      call s7(cc, ccc)
      
      if ( ccc .ne. repeat('AbC',29) .or. cc .ne. 'Ef' ) error stop 255

!     s8:
      
      call s8(repeat('Ejk',29),'AbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbC')
      
      call s8('EjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjk','AbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbCAbC')
      
      call s8('EjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjkEjk',ccc)
      
      if ( ccc .ne. repeat('AbC',29) ) error stop 244
      
      call s8(xx, ccc)
      
      if ( ccc .ne. repeat('AbC',29) .or. xx .ne. repeat('Ejk',29) ) error stop 255

      contains
      subroutine s1(x)
        character(4), value :: x
        if ( x .ne. 'AbCd' ) error stop 1
        x = 'zyZY'
      end subroutine
      subroutine s2(x, y)
        character(4), value :: x
        character(2), value :: y
        if ( x .ne. 'AbCd' ) error stop 12
        if ( y .ne. 'Ef' ) then
           error stop 22
        endif
        x = 'ZyzY'
        y = '12'
      end subroutine
      subroutine s3(x, y)
        character(1), value :: x
        character(2), value :: y
        if ( x .ne. 'A' ) error stop 13
        if ( y .ne. 'Ef' ) then
           error stop 23
        endif
        x = 'Z'
        y = '12'
      end subroutine
      subroutine s4(x, y)
        character(1), value :: x
        character(1), value :: y
        if ( x .ne. 'A' ) error stop 14
        if ( y .ne. 'E' ) then
           error stop 24
        endif
        x = 'Z'
        y = '1'
      end subroutine
      subroutine s5(x, y)
        character(87), value :: x
        character(2), value :: y
        if ( x .ne. repeat('AbC', 29) ) error stop 15
        if ( y .ne. 'Ef' ) then
           error stop 25
        endif
        x = repeat('Zyz', 29)
        y = '12'
      end subroutine
      subroutine s6(x, y)
        character(87), value :: x
        character(1), value :: y
        if ( x .ne. repeat('AbC', 29) ) error stop 16
        if ( y .ne. 'E' ) then
           error stop 26
        endif
        x = repeat('Zyz', 29)
        y = '1'
      end subroutine
      subroutine s7(x, y)
        character(1), value :: x
        character(87), value :: y
        if ( y .ne. repeat('AbC', 29) ) error stop 17
        if ( x .ne. 'E' ) then
           error stop 27
        endif
        x = 'Z'
        y = repeat('123',29)
      end subroutine
      subroutine s8(x, y)
        character(87), value :: x
        character(87), value :: y
        if ( y .ne. repeat('AbC', 29) ) error stop 17
        if ( x .ne. repeat('Ejk',29) ) then
           error stop 28
        endif
        x = repeat('987',29)
        y = repeat('123',29)
      end subroutine

end program

