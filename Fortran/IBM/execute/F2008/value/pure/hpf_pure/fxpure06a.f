@process free(f90)
!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: PROCS=4
! %COMPOPTS: -qarch=com
! %GROUP: fxpure06a.f
! %VERIFY:
! %STDIN:
! %STDOUT: fxpure06a.out
! %EXECARGS:
! %POSTCMD: spiff -r 0.0001 fxpure06a.out $TR_SRC/fxpure06a.vf
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxpure06a
!*
!*  DATE                       : Oct 26, 1995
!*  ORIGIN                     : PPS Languages, Poughkeepsie, NY
!*
!*  PRIMARY FUNCTIONS TESTED   : PURE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Verify that a pure procedure which uses
!*                               derived types and internal i/o is
!*                               accepted and returns correct results
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS            : ??
!*  STATUS                     :
!*
!*  STRUCTURE                  : Main program
!*  EXECUTABLE                 : Yes
!*
!*  INPUTS                     : None
!*  OUTPUTS                    : None
!*
!*  SETUP REQUIREMENTS         : N/A
!*  DEPENDENCIES               : None
!*
!*  REQUIRED COMPILER OPTIONS  : None
!*
!*  NORMAL COMPLETION          : Return code = 0
!*  ABNORMAL COMPLETION        : Return code ^= 0
!*
!*  RUN TIME ESTIMATE          : <60 SECS
!*
!*  ASSUMPTIONS                : None
!*
!*  CONDITIONS TESTED          :
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/26/95   MS     -Initial Version
!*  12/01/10   GB     -Copy from $(tsrcdir)hpf_pure/*.f per feature 384867
!*                     changing intent(in) to value for non arrays dummy args
!* ===================================================================
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      program fxpure06a
      type t1
        sequence
        real*8       r
        integer      i
        character*20 c
      end type t1
      type t2
        sequence
        real*16      r
        integer*8    i
        character*40 c
      end type t2

      type (t1) t1a (20), t1b (12), t1res (9)
      type (t2) t2a (20), t2b (20)
!hpf$ processors P(4)
!hpf$ distribute t1a (BLOCK) onto P
!hpf$ align t1b (b) with t1a (15 - b)
!hpf$ distribute t2b (CYCLIC) onto P
!hpf$ align with t2b :: t2a
      character*40 ct

      interface
        pure type (t1) function func1 (a, x)
          type t1
            sequence
            real*8       r
            integer      i
            character*20 c
          end type t1
          type (t1), intent(in) :: a (12)
          integer, value :: x
        end function func1

        pure subroutine subr2 (a, b, x)
          type t1
            sequence
            real*8       r
            integer      i
            character*20 c
          end type t1
          type (t1), intent (in) :: a (20)
          type (t1), intent (out) :: b (3)
          integer, value :: x
        end subroutine subr2

        pure function func3 (a, x)
          type t2
            sequence
            real*16      r
            integer*8    i
            character*40 c
          end type t2
          type (t2), dimension (5) :: func3
          type (t2), intent (in) :: a (20)
          integer, value :: x
        end function func3

        pure subroutine subr4 (a, b, x)
          type t2
            sequence
            real*16      r
            integer*8    i
            character*40 c
          end type t2
          type (t2), intent (in) :: a (20)
          type (t2), intent (inout) :: b (10)
          integer, value :: x
        end subroutine subr4
      end interface

      do i = 1, 12
        t1b (i)%i = i * 3 + 1
        t1b (i)%r = i * 1.27 - 6.24
        write (ct, fmt='(i7, f13.4)') t1b(i)%i, t1b(i)%r
        t1b (i)%c = ct
      end do
      do i = 1, 20
        t1a (i)%i = i * 4 - 7
        t2a (i)%i = i * 13 + 3
        t2b (i)%i = 48 - i * 3
        t1a (i)%r = i * .087 + 3.8
        t2a (i)%r = i * i / 65.7
        t2b (i)%r = 98.7 / i
        write (ct, fmt='(i6, f14.6)') t1a(i)%i, t1a(i)%r
        t1a (i)%c = ct
        write (ct, fmt='(i9, 9x, f22.7)') t2a(i)%i, t2a(i)%r
        t2a (i)%c = ct
        write (ct, fmt='(i9, 13x, f18.5)') t2b(i)%i, t2b(i)%r
        t2b (i)%c = ct
      end do

      print *, 'fxpure06a started'

      forall (i=1:9)
        t1res (i) = func1 (t1b, i)
      end forall
      print *, 'fxpure06a-1:'
      write (6, fmt = '(9i6)') t1res%i
      write (6, fmt = '(9f8.2)') t1res%r
      write (6, fmt = '(a20, 2x, a20, 2x, a20)') t1res%c

      forall (i=1:9)
        t1res (i) = func1 (t1a (i+11:i:-1), i)
      end forall
      print *, 'fxpure06a-2:'
      write (6, fmt = '(9i6)') t1res%i
      write (6, fmt = '(9f8.2)') t1res%r
      write (6, fmt = '(a20, 2x, a20, 2x, a20)') t1res%c

!hpf$ independent
      do i = 1, 10, 3
        call subr2 (t1a, t1b (i:i+2), i)
      end do
      print *, 'fxpure06a-3:'
      write (6, fmt = '(12i6)') t1b%i
      write (6, fmt = '(6f8.2)') t1b%r
      write (6, fmt = '(a20, 2x, a20)') t1b%c

! Now sliced.
!hpf$ independent
      do i = 1, 4
        call subr2 (t1a, t1b (i+8:i:-4), i)
      end do
      print *, 'fxpure06a-4:'
      write (6, fmt = '(12i6)') t1b%i
      write (6, fmt = '(6f8.2)') t1b%r
      write (6, fmt = '(a20, 2x, a20)') t1b%c

! Try func3, a function returning an array.
      forall (i=1:16:5)
        t2b (i : i + 4) = func3 (t2a, i)
      end forall
      print *, 'fxpure06a-5:'
      write (6, fmt = '(10i7)') t2b%i
      write (6, fmt = '(7f10.3)') t2b%r
      write (6, fmt = '(a40)') t2b%c

! Pass array in backwards, out sliced.
      forall (i=1:4)
        t2a (i:i+16:4) = func3 (t2b (20:1:-1), i)
      end forall
      print *, 'fxpure06a-6:'
      write (6, fmt = '(10i7)') t2a%i
      write (6, fmt = '(7f10.3)') t2a%r
      write (6, fmt = '(a40)') t2a%c

! Better fix the arrays after that.  Same values as before.
      do i = 1, 20
        t2a (i)%i = i * 13 + 3
        t2b (i)%i = 48 - i * 3
        t2a (i)%r = i * i / 65.7
        t2b (i)%r = 98.7 / i
        write (ct, fmt='(i9, 9x, f22.7)') t2a(i)%i, t2a(i)%r
        t2a (i)%c = ct
        write (ct, fmt='(i9, 13x, f18.5)') t2b(i)%i, t2b(i)%r
        t2b (i)%c = ct
      end do

! Call subr4, a subroutine with an inout argument.

!hpf$ independent
      do i = 1, 11, 10
        call subr4 (t2a, t2b (i:i+9), i+2)
      end do
      print *, 'fxpure06a-7:'
      write (6, fmt = '(10i7)') t2b%i
      write (6, fmt = '(7f10.3)') t2b%r
      write (6, fmt = '(a40)') t2b%c

!hpf$ independent
      do i = 1, 2
        call subr4 (t2b, t2a (i:i+18:2), i+11)
      end do
      print *, 'fxpure06a-8:'
      write (6, fmt = '(10i7)') t2a%i
      write (6, fmt = '(7f10.3)') t2a%r
      write (6, fmt = '(a40)') t2a%c

      print *, 'fxpure06a complete'
      end program

      pure type (t1) function func1 (a, x)
        type t1
          sequence
          real*8       r
          integer      i
          character*20 c
        end type t1
        type (t1), intent (in) :: a (12)
        integer, value :: x
        character*6 ct

        func1%i = a(x)%i + a(x+1)%r + 2
        func1%r = a(x+2)%r + a(x+1)%i + 6.9
        do j = 1, 20
          func1%c (j:j) = a(j/2+2)%c(21-j:21-j)
        end do
        write (ct, fmt='(i6)') a(x)%i
        func1%c (8:13) = ct
      end function func1

      pure subroutine subr2 (a, b, x)
        type t1
          sequence
          real*8       r
          integer      i
          character*20 c
        end type t1
        type (t1), intent (in) :: a (20)
        type (t1), intent (out) :: b (3)
        integer, value :: x
        character*12 ct

        do k = 1, 3
          b(k)%i = a(x)%i + a(x+k)%r + 4
          b(k)%r = a(x*2-1)%r + a(x-k+5)%i + 7.32
          do j = 1, 20
            b(k)%c (j:j) = a(21-j)%c(j:j)
          end do
          write (ct, fmt='(f12.3)') a(x+k)%r
          b(k)%c (5:16) = ct
        end do
      end subroutine subr2

      pure function func3 (a, x)
        type t2
          sequence
          real*16      r
          integer*8    i
          character*40 c
        end type t2
        type (t2), dimension (5) :: func3
        type (t2), intent (in) :: a (20)
        integer, value :: x
        character*40 ct

        do k = 1, 5
          func3(k)%i = a(x+k-1)%i + a(x+1)%r + 7
          func3(k)%r = a(k*2)%r + a(x-k+5)%i + 3.08
          write (ct, fmt='(20a2)') a%c(14:15)
          func3(k)%c = ct
          write (ct, fmt='(f13.4)') a(k-x+16)%r
          func3(k)%c (9:21) = ct
        end do
      end function func3

      pure subroutine subr4 (a, b, x)
        type t2
          sequence
          real*16      r
          integer*8    i
          character*40 c
        end type t2
        type (t2), intent (in) :: a (20)
        type (t2), intent (inout) :: b (10)
        integer, value :: x
        character*9 ct

        do k = 1, 10
          b(k)%i = a(k)%i + b(x/2+3)%i - a(x+k-3)%r + 4
          b(k)%r = a(x-1)%r / b(k)%r + a(x-k+8)%i + 9.911
          write (ct, fmt='(f9.3)') a(x)%r
          b(k)%c (4:12) = ct
          write (ct, fmt='(f9.4)') b(11-k)%r
          b(k)%c (17:25) = ct
        end do
      end subroutine subr4
