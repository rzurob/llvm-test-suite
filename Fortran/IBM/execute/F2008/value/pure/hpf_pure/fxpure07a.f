@process free(f90)
!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct 26, 1995
!*
!*  PRIMARY FUNCTIONS TESTED   : PURE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Verify that a pure procedure which uses
!*                               derived types works with allocatable
!*                               arrays and host-association.
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
!*
!* ===================================================================
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      program fxpure07a
      type t1
        sequence
        real*8  r
        integer i (3)
      end type t1
      type t2
        sequence
        real*16   r
        integer*8 i
      end type t2

      type (t1), allocatable :: t1a (:, :), t1b (:, :)
      type (t1) t1res (12)
      type (t2), dimension (:, :), allocatable :: t2a, t2b
!hpf$ processors P(2, 2)
!hpf$ distribute t1a (BLOCK, CYCLIC (2)) onto P
!hpf$ align with t1a :: t1b
!hpf$ distribute t2b (CYCLIC, BLOCK) onto P
!hpf$ align t2a (k, j) with t2b (j, k)

      interface
        pure function func3 (a, x)
          type t2
            sequence
            real*16   r
            integer*8 i
          end type t2
          type (t2), intent (in) :: a (:, :)
          type (t2), dimension (size (a, 2)) :: func3
          integer, value :: x
        end function func3

        pure subroutine subr4 (a, b, x)
          type t2
            sequence
            real*16   r
            integer*8 i
          end type t2
          type (t2), intent (in) :: a (:, :)
          type (t2), intent (inout) :: b (:)
          integer, value :: x
        end subroutine subr4
      end interface

      allocate (t1a (12, 8))
      allocate (t1b (12, 8))

      allocate (t2b (6, 16))
      allocate (t2a (16, 6))

      do i = 1, 12
        do j = 1, 8
          t1a (i, j)%i (1) = i * 6 - 7 * j
          t1a (i, j)%i (2) = i * j * 7 - 17
          t1a (i, j)%i (3) = 477 - i * j / 4
          t1a (i, j)%r = i * .087 + 3.8 * j
          t1b (i, j)%i (1) = i * 3 + j * 12
          t1b (i, j)%i (2) = i * 4 + j * 5
          t1b (i, j)%i (3) = i * 5 - j * 8
          t1b (i, j)%r = i * j * 1.27 - 276.24
        end do
      end do
      do i = 1, 16
        do j = 1, 6
          t2a (i, j)%i = 548 - i * 13 - j * 12
          t2a (i, j)%r = 98.7 / i + j * 11.1
        end do
      end do

      print *, 'fxpure07a started'

      forall (i=1:12)
        t1res (i) = func1 (t1b, i)
      end forall
      print *, 'fxpure07a-1:'
      write (6, fmt = '(2(2x, 3i10))') (t1res (k)%i, k = 1,12)
      write (6, fmt = '(6f10.2)') t1res%r

! Sections of differing sizes passed in forall loop.
      forall (i=1:12)
        t1res (i) = func1 (t1a (i/3+1:i/2+6, :), i)
      end forall
      print *, 'fxpure07a-2:'
      write (6, fmt = '(3(2x, 3i8))') (t1res (k)%i, k = 1,12)
      write (6, fmt = '(6f8.2)') t1res%r

!hpf$ independent
      do i = 1, 8
        call subr2 (t1a, t1b (:, i), i+3)
      end do
      print *, 'fxpure07a-3:'
      write (6, fmt = '(3(2x, 3i7))') ((t1b(j,k)%i, j=1,12), k=1,8)
      write (6, fmt = '(6f8.2)') t1b%r

!hpf$ independent
      do i = 1, 12
        call subr2 (t1b, t1a (i, :), i)
      end do
      print *, 'fxpure07a-4:'
      write (6, fmt = '(2(2x, 3i11))') ((t1a(j,k)%i, j=1,12), k=1,8)
      write (6, fmt = '(6f8.2)') t1a%r

! Try func3, a function returning an unknown size array.
      forall (i=1:16)
        t2b (:, i) = func3 (t2a, i)
      end forall
      print *, 'fxpure07a-5:'
      write (6, fmt = '(8i8)') t2b%i
      write (6, fmt = '(4f11.3)') t2b%r

! Pass array in backwards
      forall (i=1:6)
        t2a (:, i) = func3 (t2b (6:1:-1, :), i)
      end forall
      print *, 'fxpure07a-6:'
      write (6, fmt = '(4i10)') t2a%i
      write (6, fmt = '(4f11.3)') t2a%r

! Better fix the arrays after that
      do i = 1, 16
        do j = 1, 6
          t2a (i, j)%i = i * 13 + 3 * j
          t2a (i, j)%r = i * j / 65.7
          t2b (j, i)%i = 21 - i * 7
          t2b (j, i)%r = i / j * 15.3
        end do
      end do

! Call subr4, a subroutine with an inout argument.

!hpf$ independent
      do i = 1, 16
        call subr4 (t2a, t2b (:, i), i)
      end do
      print *, 'fxpure07a-7:'
      write (6, fmt = '(8i8)') t2b%i
      write (6, fmt = '(8f9.3)') t2b%r

! Pass along other dimension
!hpf$ independent
      do i = 1, 6
        call subr4 (t2a, t2b (i, :), i)
      end do
      print *, 'fxpure07a-8:'
      write (6, fmt = '(4i10)') t2b%i
      write (6, fmt = '(8f9.3)') t2b%r

      print *, 'fxpure07a complete'

      contains
      pure type (t1) function func1 (a, x)
        type (t1), intent (in) :: a (:, :)
        integer, value :: x
        integer m, n, q (3)
        real*8 s

        m = ubound (a, 1)
        n = ubound (a, 2)

        q = 0
        s = 0.
        do ii = 1, m
          do jj = 1, n
            do kk = 1, 3
              q (kk) = q (kk) * (9+x)
              q (kk) = q(kk)/(10+x) + a(ii,jj)%i(kk)*267
            end do
            s = s * .9 + a(ii,jj)%r
          end do
        end do
        func1%i = q
        func1%r = s * x
      end function func1

      pure subroutine subr2 (a, b, x)
        type (t1), intent (in) :: a (:, :)
        type (t1), intent (out) :: b (:)
        integer, value :: x
        integer m, n, p, q (3)
        real*8 s

        m = ubound (a, 1)
        n = ubound (a, 2)
        p = ubound (b, 1)

        do ii = 1, p
          q = 0
          s = 0.
          do jj = ii, ii + m
            jm = mod (jj, m) + 1
            jn = mod (jj+jj, n) + 1
            do kk = 1, 3
              q (kk) = q (kk) * (9+x)
              q (kk) = q(kk)/(10+x) + a(jm,jn)%i(kk)*137
            end do
            s = s * .93 + a(jm,jn)%r
          end do
          b(ii)%i = q
          b(ii)%r = s
        end do
      end subroutine subr2

      end program

      pure function func3 (a, x)
        type t2
          sequence
          real*16   r
          integer*8 i
        end type t2
        type (t2), intent (in) :: a (:, :)
        type (t2), dimension (size (a, 2)) :: func3
        integer, value :: x
        integer m, n, q
        real*8 s

        m = ubound (a, 1)
        n = ubound (a, 2)

        do i = 1, n
          q = 0
          s = 0.
          do j = 1, m
              q = q * (19+x)
              q = q / (20+x) + a(j,i)%i * 367
            s = s * .94 + a(j,i)%r
          end do
          func3 (i)%i = q
          func3 (i)%r = s * x
        end do
      end function func3

      pure subroutine subr4 (a, b, x)
        type t2
          sequence
          real*16   r
          integer*8 i
        end type t2
        type (t2), intent (in) :: a (:, :)
        type (t2), intent (inout) :: b (:)
        integer, value :: x
        integer m, n, p, q
        real*8 s

        m = ubound (a, 1)
        n = ubound (a, 2)
        p = ubound (b, 1)

        do i = 1, p
          q = 0
          s = 0.
          do j = i, i + m
            jm = mod (j, m) + 1
            jn = mod (j+j, n) + 1
              q = q * (29+x)
              q = q / (30+x) + a(jm,jn)%i * 74 - b(i)%i * 34
            s = s * .86 + a(jm,jn)%r
          end do
          b(i)%i = q
          b(i)%r = s
        end do
      end subroutine subr4
