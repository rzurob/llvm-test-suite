!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP:  elem15e.f
! %VERIFY: elem15e.out:elem15e.vf
! %STDIN:
! %STDOUT: elem15e.out
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ELEMENTAL
!*
!*  PROGRAMMER                 : Cindy Chow
!*  DATE                       : September 13, 1998
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-defined ELEMENTAL procedures.
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : See text below.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  09/13/98   CC     -Initial release
!*  12/01/10   GB     -Copy from $(tsrcdir)elemental/unit/*.f - feature 384867
!*                     changing intent(in) to value for non arrays dummy args
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
!-----------------------------------------------------------------------!
! Test: External elemental function with ENTRY and character arguments. !
!                                                                       !
! Note: Actual arguments are assumed-shape arrays.                      !
!-----------------------------------------------------------------------!

      program elem15e
         interface
            subroutine mainsub ( arr1d, brr1d, xrr1d, yrr1d, res1d,           &
     &                           arr2d, brr2d, xrr2d, yrr2d, res2d,           &
     &                           arr3d, brr3d, xrr3d, yrr3d, res3d )
               character(5) :: arr1d(:), arr2d(:,:), arr3d(:,:,:)
               character(5) :: brr1d(:), brr2d(:,:), brr3d(:,:,:)
               character(5) :: xrr1d(:), xrr2d(:,:), xrr3d(:,:,:)
               character(5) :: yrr1d(:), yrr2d(:,:), yrr3d(:,:,:)
               character(5) :: res1d(:), res2d(:,:), res3d(:,:,:)
            end subroutine mainsub
         end interface

         character(5) :: arr1d(5), arr2d(4,4), arr3d(2,2,2)
         character(5) :: brr1d(5), brr2d(4,4), brr3d(2,2,2)
         character(5) :: xrr1d(5), xrr2d(2,2), xrr3d(2,2,2)
         character(5) :: yrr1d(5), yrr2d(2,2), yrr3d(2,2,2)
         character(5) :: res1d(5), res2d(2,2), res3d(2,2,2)

         call mainsub ( arr1d, brr1d, xrr1d, yrr1d, res1d,                    &
     &                  arr2d, brr2d, xrr2d, yrr2d, res2d,                    &
     &                  arr3d, brr3d, xrr3d, yrr3d, res3d )
      end program elem15e


      subroutine mainsub ( arr1d, brr1d, xrr1d, yrr1d, res1d,                 &
     &                     arr2d, brr2d, xrr2d, yrr2d, res2d,                 &
     &                     arr3d, brr3d, xrr3d, yrr3d, res3d )
         interface
            elemental function fnc (a, b)
               character(5), value :: a
               character(5), value :: b
               character(5)             :: fnc
            end function fnc

            elemental function ent (a, b)
               character(5), value :: a
               character(5), value :: b
               character(5)             :: ent
            end function ent
         end interface

         character(5) :: ace, arr1d(:), arr2d(:,:), arr3d(:,:,:)
         character(5) :: bce, brr1d(:), brr2d(:,:), brr3d(:,:,:)
         character(5) :: xce, xrr1d(:), xrr2d(:,:), xrr3d(:,:,:)
         character(5) :: yce, yrr1d(:), yrr2d(:,:), yrr3d(:,:,:)
         character(5) :: res, res1d(:), res2d(:,:), res3d(:,:,:)

         character(5), parameter :: s0 = 'apple',                             &
     &                              s1 = 'candy',                             &
     &                              s2 = 'maple',                             &
     &                              s3 = 'sugar',                             &
     &                              s4 = 'fudge',                             &
     &                              s5 = 'fuzzy',                             &
     &                              s6 = 'peach',                             &
     &                              s7 = 'wagon',                             &
     &                              s8 = 'wheel',                             &
     &                              s9 = 'fruit'

! Actual arguments are scalar expressions.

         ace = 'apple' // 'candy'
         bce = 'fudge'
         xce = ace(1:2) // bce(3:5)
         yce = ace(1:3) // bce(4:5)

         res = fnc ('apple'//'candy', 'fudge')
         if (res .ne. xce) error stop 101
         print '(A8)', res
         res = ent ('apple'//'candy', 'fudge')
         if (res .ne. yce) error stop 102
         print '(A8)', res

! Actual arguments are scalar variables.

         ace = 'maple'
         bce = 'sugar'
         xce = ace(1:2) // bce(3:5)
         yce = ace(1:3) // bce(4:5)

         res = fnc (ace, bce)
         if (res .ne. xce) error stop 103
         print '(A8)', res
         res = ent (ace, bce)
         if (res .ne. yce) error stop 104
         print '(A8)', res

! Actual arguments are one-dimension arrays.

         arr1d = (/ s1,s2,s3,s4,s5 /)
         brr1d = (/ s5,s6,s7,s8,s9 /)
         xrr1d(1:5) = arr1d(1:5)(1:2) // brr1d(1:5)(3:5)
         yrr1d(1:5) = arr1d(1:5)(1:3) // brr1d(1:5)(4:5)

         res1d = fnc (arr1d, brr1d)
         if (any(res1d .ne. xrr1d)) error stop 105
         print '(5(A8))', res1d
         res1d = ent (arr1d, brr1d)
         if (any(res1d .ne. yrr1d)) error stop 106
         print '(5(A8))', res1d

! Actual arguments are three-dimension arrays.

         arr3d = reshape ( (/s1,s2,s3,s4,s5,s6,s7,s8/), (/2,2,2/) )
         brr3d = reshape ( (/s2,s3,s4,s5,s6,s7,s8,s9/), (/2,2,2/) )
         xrr3d(1:2,1:2,1:2) = arr3d(1:2,1:2,1:2)(1:2) //                      &
     &                        brr3d(1:2,1:2,1:2)(3:5)
         yrr3d(1:2,1:2,1:2) = arr3d(1:2,1:2,1:2)(1:3) //                      &
     &                        brr3d(1:2,1:2,1:2)(4:5)

         res3d = fnc (arr3d, brr3d)
         if (any(res3d .ne. xrr3d)) error stop 107
         print '(8(A8))', res3d
         res3d = ent (arr3d, brr3d)
         if (any(res3d .ne. yrr3d)) error stop 108
         print '(8(A8))', res3d

! Actual arguments are two-dimension array sections.

         k1 = 1
         k2 = 2
         k3 = 3
         k4 = 4

         arr2d = reshape ( (/s1,s2,s3,s4,s5,s6,s7,s8,                         &
     &                       s9,s0,s1,s2,s3,s4,s5,s6/),                       &
     &                     (/4,4/) )
         brr2d = reshape ( (/s0,s9,s8,s7,s6,s5,s4,s3,                         &
     &                       s2,s1,s0,s9,s8,s7,s6,s5/),                       &
     &                     (/4,4/) )
         xrr2d = arr2d(1:2,3:4)(1:2) // brr2d(3:4,1:2)(3:5)
         yrr2d = arr2d(1:2,2:3)(1:3) // brr2d(2:3,3:4)(4:5)

         res2d = fnc (arr2d(k1:2,3:k4), brr2d(3:k4,k1:2))
         if (any(res2d .ne. xrr2d)) error stop 109
         print '(4(A8))', res2d
         res2d = ent (arr2d(1:2,k2:k3), brr2d(k2:k3,3:4))
         if (any(res2d .ne. yrr2d)) error stop 110
         print '(4(A8))', res2d

! Actual arguments are one-dimension array constructors.

         arr1d = (/ s1,s3,s5,s7,s9 /)
         brr1d = (/ s2,s4,s6,s8,s0 /)
         xrr1d(1:5) = arr1d(1:5)(1:2) // brr1d(1:5)(3:5)
         yrr1d(1:5) = arr1d(1:5)(1:3) // brr1d(1:5)(4:5)

         res1d = fnc ( (/ s1,s3,s5,s7,s9 /), (/ s2,s4,s6,s8,s0 /) )
         if (any(res1d .ne. xrr1d)) error stop 111
         print '(5(A8))', res1d
         res1d = ent ( (/ s1,s3,s5,s7,s9 /), (/ s2,s4,s6,s8,s0 /) )
         if (any(res1d .ne. yrr1d)) error stop 112
         print '(5(A8))', res1d

! Actual arguments are three-dimension array constructors.

         arr3d = reshape ( (/s1,s2,s3,s4,s5,s6,s7,s8/), (/2,2,2/) )
         brr3d = reshape ( (/s1,s3,s5,s7,s2,s4,s6,s8/), (/2,2,2/) )
         xrr3d(1:2,1:2,1:2) = arr3d(1:2,1:2,1:2)(1:2) //                      &
     &                        brr3d(1:2,1:2,1:2)(3:5)
         yrr3d(1:2,1:2,1:2) = arr3d(1:2,1:2,1:2)(1:3) //                      &
     &                        brr3d(1:2,1:2,1:2)(4:5)

         res3d = fnc ( reshape((/s1,s2,s3,s4,s5,s6,s7,s8/), (/2,2,2/)),       &
     &                 reshape((/s1,s3,s5,s7,s2,s4,s6,s8/), (/2,2,2/)) )
         if (any(res3d .ne. xrr3d)) error stop 113
         print '(8(A8))', res3d
         res3d = ent ( reshape((/s1,s2,s3,s4,s5,s6,s7,s8/), (/2,2,2/)),       &
     &                 reshape((/s1,s3,s5,s7,s2,s4,s6,s8/), (/2,2,2/)) )
         if (any(res3d .ne. yrr3d)) error stop 114
         print '(8(A8))', res3d

      end subroutine mainsub


!-----------------------------------------------------------------!
! External elemental function with ENTRY and character arguments. !
!-----------------------------------------------------------------!

      elemental function fnc (a, b)
         character(5), value :: a
         character(5), value :: b
         character(5)             :: fnc
         character(5)             :: ent
         fnc = a(1:2) // b(3:5)
         return
      entry ent (a, b)
         ent = a(1:3) // b(4:5)
         return
      end function fnc
