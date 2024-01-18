@process free(f90)
      program fxpure12a
!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: PROCS=4
! %COMPOPTS:
! %GROUP: fxpure12a.f
! %VERIFY: fxpure12a.out:fxpure12a.vf
! %STDIN:
! %STDOUT: fxpure12a.out
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxpure12a
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Mike Schouten
!*  DATE                       : Mar 16, 1995
!*  ORIGIN                     : PPS Languages, Kingston, NY
!*
!*  PRIMARY FUNCTIONS TESTED   : PURE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Verify that a pure procedure which has the
!*                               RECURSIVE prefix spec is accepted and returns
!*                               the correct results.
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
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/16/95   MS     -Initial Version
!*  12/01/10   GB     -Copy from $(tsrcdir)hpf_pure/*.f per feature 384867
!*                     changing intent(in) to value for non arrays dummy args
!*
!* ===================================================================
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      real*4, dimension (50) :: r1, r2
      integer, dimension (8) :: fibar

!hpf$ processors P(4)
!hpf$ distribute r1 (BLOCK) onto P
!hpf$ distribute r2 (CYCLIC) onto P
!hpf$ distribute (BLOCK) onto P :: fibar

      interface
        pure recursive subroutine pusort (a)
          real*4, intent (inout), dimension (:) :: a
        end subroutine pusort

        pure recursive function fib (iv) result (rout)
          integer rout
          integer, value :: iv
        end function fib
      end interface

      print *, 'fxpure12a started'

! pump some random stuff into the arrays

      r1 = (/  114.3, -91.7, -68.9,   9.0,  34.8,   0.0, 712.0, -805.0, &
        -89.5, 109.3, -91.0,  -7.7, 717.2,  32.5, -43.3, -999.9,  12.2, &
       -394.2,  76.9, -28.1,  84.3,  -1.5, 283.5, 271.2, -18.4,  -88.4, &
       -717.2, 712.0,  55.5, 555.5, 261.6,  38.0,   -.2,    .9, -314.1, &
       1234.5,  13.2, 271.8, 141.4, 173.2, 800.0,  80.2, -90.5, 3141.5, &
          1.6,   3.2,  -7.4, -31.9, -92.6,   3.9/)
      r2 = r1

! Test #1:  Try sorting the block array

! This test seems too simple.  I would rather it made a difference that
! the subroutine is declared pure.
      
      call pusort (r1)
      
      do i = 1, 46, 5
        write(6, 10000) 1, r1 (i), r1 (i+1), r1 (i+2), r1 (i+3), r1 (i+4)
      end do

! Test #2:  Cyclic distribution

      call pusort (r2)
      
      do i = 1, 46, 5
        write(6, 10000) 2, r2 (i), r2 (i+1), r2 (i+2), r2 (i+3), r2 (i+4)
      end do

! Test #3:  Fib code

      forall (i = 1:8) fibar (i) = fib (i)
      do i = 1, 7, 2
        write(6,10010) 3, fibar (i), fibar (i+1)
      end do

10000 format(' fxpure12a-', i1, ': ', f7.2, f7.2, f7.2, f7.2, f7.2)
10010 format(' fxpure12a-', i1, ': ', i4, i4)
!
!     End of test case
!
      end program

      pure recursive subroutine pusort (a)
        real*4, intent (inout), dimension (:) :: a
        real*4  loc (ubound (a, 1))
        i = ubound (a, 1)
        if (i .gt. 2) then
          j = i / 2
          call pusort (a (1:j))
          call pusort (a (j+1:i))
          k = i          ! perform a merge
          mid = j
          do while (i .gt. mid .and. j .gt. 0)
            if (a (i) .gt. a (j)) then
              loc (k) = a (i)
              i = i - 1
            else
              loc (k) = a (j)
              j = j - 1
            end if
            k = k - 1
          end do
          if (i .eq. mid) then     ! if i finished first
            mid = 0
            i = j
          end if
          do while (i .gt. mid)
            loc (k) = a (i)
            i = i - 1
            k = k - 1
          end do
          a = loc
        else
          if (i .eq. 2 .and. a (1) .gt. a (2)) then
            t = a (1)
            a (1) = a (2)
            a (2) = t
          end if
        end if
      end subroutine pusort

      pure recursive function fib (iv) result (rout)
        integer rout
        integer, value :: iv
        integer, automatic :: output
        if (iv .gt. 1) then
          output = fib (iv - 1) + fib (iv - 2)
        else
          output = 1
        end if
        rout = output
      end function fib
