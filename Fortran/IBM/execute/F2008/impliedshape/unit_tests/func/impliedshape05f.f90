!* =================================================================== &
!*
!* DATE                       : February 14, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              implied-shape arrays of character(*)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program impliedshape05f

      character(*), parameter :: ch4f1 (*) = &
      & [ 'abcabcdabcd', ('bcde       ',i=2,6) ]

      character(*), parameter :: ch4f2 (*) = &
      & [    ('aaa',i=1,5), 'bbb' ]

      character(*), parameter :: ch4f3 (*) = &
      & [ 'how', ('are',i=2,5), 'you' ]

      character(*), parameter :: ch4f4 (*) = &
      & ch4f1

      character(*), parameter :: ch4f5 (*) = &
      & reshape(['reshaping to [1] ','SHOULD NOT APPEAR'],[1])

      character(*), parameter :: ch4f6 (*,*) = &
      & reshape(['a','b','c','d'],[2,2])

      character(*), parameter :: ch4f7 (*,*,*) = &
      & reshape(['a ','b ','c ','d ','e ','f ','g ','h '],[2,2,2])

      character(KIND=1,LEN=*), parameter :: ch4f8 (*,*,*,*) = &
      & reshape(['aa   ','bb   ','ccc  ','dddd ','eeeee', &
      &          'f    ','g    ','h    ','ii   ','j    ', &
      &          'k    ','l    ','mm   ','n    ','o    ', &
      &          'p    '],[2,2,2,2])

      character(*), parameter :: ch4f9 (*) = &
      & [ character(8) :: ('testing ',i=1,2), '1','2','3' ]

      ! Explicit
      character(*), parameter :: ch4f1a (6) = &
      & [ 'abcabcdabcd', ('bcde       ',i=2,6) ]

      character(*), parameter :: ch4f2a (6) = &
      & [    ('aaa',i=1,5), 'bbb' ]

      character(*), parameter :: ch4f3a (6) = &
      & [ 'how', ('are',i=2,5), 'you' ]

      character(*), parameter :: ch4f4a (6) = &
      & ch4f1a

      character(*), parameter :: ch4f5a (1) = &
      & reshape(['reshaping to [1] ','SHOULD NOT APPEAR'],[1])

      character(*), parameter :: ch4f6a (2,2) = &
      & reshape(['a','b','c','d'],[2,2])

      character(*), parameter :: ch4f7a (2,2,2) = &
      & reshape(['a ','b ','c ','d ','e ','f ','g ','h '],[2,2,2])

      character(KIND=1,LEN=*), parameter :: ch4f8a (2,2,2,2) = &
      & reshape([character(5) :: &
      &          'aa','bb','ccc','dddd','eeeee','f','g','h', &
      &          'ii','j','k','l','mm','n','o','p'],[2,2,2,2])

      character(*), parameter :: ch4f9a (5) = &
      & [ ('testing ',i=1,2), '1       ','2       ','3       ' ]


      if (ANY(ch4f1 .NE. ch4f1a)) ERROR STOP 1
      if (ANY(ch4f2 .NE. ch4f2a)) ERROR STOP 2
      if (ANY(ch4f3 .NE. ch4f3a)) ERROR STOP 3
      if (ANY(ch4f4 .NE. ch4f4a)) ERROR STOP 4
      if (ANY(ch4f5 .NE. ch4f5a)) ERROR STOP 5
      if (ANY(ch4f6 .NE. ch4f6a)) ERROR STOP 6
      if (ANY(ch4f7 .NE. ch4f7a)) ERROR STOP 7
      if (ANY(ch4f8 .NE. ch4f8a)) ERROR STOP 8
      if (ANY(ch4f9 .NE. ch4f9a)) ERROR STOP 9

      call test_charstar(ch4f1,ch4f1a)
      call test_charstar(ch4f2,ch4f2a)
      call test_charstar(ch4f3,ch4f3a)
      call test_charstar(ch4f4,ch4f4a)
      call test_charstar(ch4f5,ch4f5a)
      call test_charstar2d(ch4f6,ch4f6a)
      call test_charstar3d(ch4f7,ch4f7a)
      call test_charstar4d(ch4f8,ch4f8a)
      call test_charauto(ch4f8,ch4f8a,len(ch4f8))

      contains
      subroutine test_charstar (c,d)
        character(*) :: c(:)
        character(*) :: d(:)
        if (ANY(c .NE. d)) ERROR STOP 11
      end subroutine test_charstar

      subroutine test_charstar2d (c, d)
        character(*) :: c(:,:)
        character(*) :: d(:,:)
        if (ANY(c .NE. d)) ERROR STOP 12
      end subroutine test_charstar2d

      subroutine test_charstar3d (c, d)
        character(*) :: c(:,:,:)
        character(*) :: d(:,:,:)
        if (ANY(c .NE. d)) ERROR STOP 13
      end subroutine test_charstar3d

      subroutine test_charstar4d (c, d)
        character(*) :: c(:,:,:,:)
        character(*) :: d(:,:,:,:)
        if (ANY(c .NE. d)) ERROR STOP 14
      end subroutine test_charstar4d

      subroutine test_charauto (c, d, n)
        character(n) :: c(:,:,:,:)
        character(n) :: d(:,:,:,:)
        if (ANY(c .NE. d)) ERROR STOP 15
      end subroutine

      end
