!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : September 13, 1998
!*
!*  PRIMARY FUNCTIONS TESTED   : User-defined ELEMENTAL procedures.
!*  SECONDARY FUNCTIONS TESTED :
!*
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
!--------------------------------------------------------------------------!
! Test: External elemental function with ENTRY and derived type arguments. !
!--------------------------------------------------------------------------!

      program elem07e
         interface
            elemental function fnc (a, b)
               type dt
                  sequence
                  integer i
                  real    r
               end type dt
               type(dt), value :: a
               type(dt), value :: b
               type(dt)             :: fnc
            end function fnc

            elemental function ent (a, b)
               type dt
                  sequence
                  integer i
                  real    r
               end type dt
               type(dt), value :: a
               type(dt), value :: b
               type(dt)             :: ent
            end function ent
         end interface

         type dt
            sequence
            integer i
            real    r
         end type dt

         type(dt) :: ace, arr1d(5), arr2d(4,4), arr3d(2,2,2)
         type(dt) :: bce, brr1d(5), brr2d(4,4), brr3d(2,2,2)
         type(dt) :: xce, xrr1d(5), xrr2d(2,2), xrr3d(2,2,2)
         type(dt) :: yce, yrr1d(5), yrr2d(2,2), yrr3d(2,2,2)
         type(dt) :: res, res1d(5), res2d(2,2), res3d(2,2,2)

         integer(4), parameter :: i0 = 0,                                     &
     &                            i1 = 1,                                     &
     &                            i2 = 2,                                     &
     &                            i3 = 3,                                     &
     &                            i4 = 4,                                     &
     &                            i5 = 5,                                     &
     &                            i6 = 6,                                     &
     &                            i7 = 7,                                     &
     &                            i8 = 8,                                     &
     &                            i9 = 9

         real(4), parameter :: r0 = 0.e0,                                     &
     &                         r1 = 1.e0,                                     &
     &                         r2 = 2.e0,                                     &
     &                         r3 = 3.e0,                                     &
     &                         r4 = 4.e0,                                     &
     &                         r5 = 5.e0,                                     &
     &                         r6 = 6.e0,                                     &
     &                         r7 = 7.e0,                                     &
     &                         r8 = 8.e0,                                     &
     &                         r9 = 9.e0

! Actual arguments are scalar expressions.

         ace = dt(12345,12.34e0)
         bce = dt(67+89,67.89e0)
         xce%i = ace%i * bce%i
         xce%r = ace%r * bce%r
         yce%i = ace%i + bce%i
         yce%r = ace%r + bce%r

         res = fnc ( dt(12345,12.34e0), dt(67+89,67.89e0) )
         if (res%i .ne. xce%i) error stop 101
         if (res%r .ne. xce%r) error stop 101
         print '(I8,F8.1)', res
         res = ent ( dt(12345,12.34e0), dt(67+89,67.89e0) )
         if (res%i .ne. yce%i) error stop 102
         if (res%r .ne. yce%r) error stop 102
         print '(I8,F8.1)', res

! Actual arguments are scalar variables.

         ace = dt(12345,12.34e0)
         bce = dt(67+89,67.89e0)
         xce%i = ace%i * bce%i
         xce%r = ace%r * bce%r
         yce%i = ace%i + bce%i
         yce%r = ace%r + bce%r

         res = fnc (ace, bce)
         if (res%i .ne. xce%i) error stop 103
         if (res%r .ne. xce%r) error stop 103
         print '(I8,F8.1)', res
         res = ent (ace, bce)
         if (res%i .ne. yce%i) error stop 104
         if (res%r .ne. yce%r) error stop 104
         print '(I8,F8.1)', res

! Actual arguments are one-dimension arrays.

         arr1d%i = (/ i1,i2,i3,i4,i5 /)
         arr1d%r = (/ r1,r2,r3,r4,r5 /)
         brr1d%i = (/ i5,i6,i7,i8,i9 /)
         brr1d%r = (/ r5,r6,r7,r8,r9 /)
         xrr1d%i = arr1d%i * brr1d%i
         xrr1d%r = arr1d%r * brr1d%r
         yrr1d%i = arr1d%i + brr1d%i
         yrr1d%r = arr1d%r + brr1d%r

         res1d = fnc (arr1d, brr1d)
         if (any(res1d%i .ne. xrr1d%i)) error stop 105
         if (any(res1d%r .ne. xrr1d%r)) error stop 105
         print '(5(I8,F8.1))', res1d
         res1d = ent (arr1d, brr1d)
         if (any(res1d%i .ne. yrr1d%i)) error stop 106
         if (any(res1d%r .ne. yrr1d%r)) error stop 106
         print '(5(I8,F8.1))', res1d

! Actual arguments are three-dimension arrays.

         arr3d%i = reshape ( (/i1,i2,i3,i4,i5,i6,i7,i8/), (/2,2,2/) )
         arr3d%r = reshape ( (/r1,r2,r3,r4,r5,r6,r7,r8/), (/2,2,2/) )
         brr3d%i = reshape ( (/i2,i3,i4,i5,i6,i7,i8,i9/), (/2,2,2/) )
         brr3d%r = reshape ( (/r2,r3,r4,r5,r6,r7,r8,r9/), (/2,2,2/) )
         xrr3d%i = arr3d%i * brr3d%i
         xrr3d%r = arr3d%r * brr3d%r
         yrr3d%i = arr3d%i + brr3d%i
         yrr3d%r = arr3d%r + brr3d%r

         res3d = fnc (arr3d, brr3d)
         if (any(res3d%i .ne. xrr3d%i)) error stop 107
         if (any(res3d%r .ne. xrr3d%r)) error stop 107
         print '(8(I8,F8.1))', res3d
         res3d = ent (arr3d, brr3d)
         if (any(res3d%i .ne. yrr3d%i)) error stop 108
         if (any(res3d%r .ne. yrr3d%r)) error stop 108
         print '(8(I8,F8.1))', res3d

! Actual arguments are two-dimension array sections.

         k1 = 1
         k2 = 2
         k3 = 3
         k4 = 4

         arr2d%i = reshape ( (/i1,i2,i3,i4,i5,i6,i7,i8,i9,i0,                 &
     &                         i1,i2,i3,i4,i5,i6/), (/4,4/) )
         arr2d%r = reshape ( (/r1,r2,r3,r4,r5,r6,r7,r8,r9,r0,                 &
     &                         r1,r2,r3,r4,r5,r6/), (/4,4/) )
         brr2d%i = reshape ( (/i0,i9,i8,i7,i6,i5,i4,i3,i2,i1,                 &
     &                         i0,i9,i8,i7,i6,i5/), (/4,4/) )
         brr2d%r = reshape ( (/r0,r9,r8,r7,r6,r5,r4,r3,r2,r1,                 &
     &                         r0,r9,r8,r7,r6,r5/), (/4,4/) )
         xrr2d%i = arr2d(1:2,3:4)%i * brr2d(3:4,1:2)%i
         xrr2d%r = arr2d(1:2,3:4)%r * brr2d(3:4,1:2)%r
         yrr2d%i = arr2d(1:2,2:3)%i + brr2d(2:3,3:4)%i
         yrr2d%r = arr2d(1:2,2:3)%r + brr2d(2:3,3:4)%r

         res2d = fnc (arr2d(k1:k2,k3:k4), brr2d(3:4,1:2))
         if (any(res2d%i .ne. xrr2d%i)) error stop 109
         if (any(res2d%r .ne. xrr2d%r)) error stop 109
         print '(4(I8,F8.1))', res2d
         res2d = ent (arr2d(1:k2,k2:3), brr2d(2:k3,k3:4))
         if (any(res2d%i .ne. yrr2d%i)) error stop 110
         if (any(res2d%r .ne. yrr2d%r)) error stop 110
         print '(4(I8,F8.1))', res2d

! Actual arguments are one-dimension array constructors.

         arr1d%i = (/ i1,i3,i5,i7,i9 /)
         arr1d%r = (/ r1,r3,r5,r7,r9 /)
         brr1d%i = (/ i2,i4,i6,i8,i0 /)
         brr1d%r = (/ r2,r4,r6,r8,r0 /)
         xrr1d%i = arr1d%i * brr1d%i
         xrr1d%r = arr1d%r * brr1d%r
         yrr1d%i = arr1d%i + brr1d%i
         yrr1d%r = arr1d%r + brr1d%r

         res1d = fnc ( (/ dt(i1,r1), dt(i3,r3), dt(i5,r5),                    &
     &                    dt(i7,r7), dt(i9,r9) /),                            &
     &                 (/ dt(i2,r2), dt(i4,r4), dt(i6,r6),                    &
     &                    dt(i8,r8), dt(i0,r0) /) )
         if (any(res1d%i .ne. xrr1d%i)) error stop 111
         if (any(res1d%r .ne. xrr1d%r)) error stop 111
         print '(5(I8,F8.1))', res1d

         res1d = ent ( (/ dt(i1,r1), dt(i3,r3), dt(i5,r5),                    &
     &                    dt(i7,r7), dt(i9,r9) /),                            &
     &                 (/ dt(i2,r2), dt(i4,r4), dt(i6,r6),                    &
     &                    dt(i8,r8), dt(i0,r0) /) )
         if (any(res1d%i .ne. yrr1d%i)) error stop 112
         if (any(res1d%r .ne. yrr1d%r)) error stop 112
         print '(5(I8,F8.1))', res1d

! Actual arguments are three-dimension array constructors.

         arr3d%i = reshape ( (/i1,i2,i3,i4,i5,i6,i7,i8/), (/2,2,2/) )
         arr3d%r = reshape ( (/r1,r2,r3,r4,r5,r6,r7,r8/), (/2,2,2/) )
         brr3d%i = reshape ( (/i1,i3,i5,i7,i2,i4,i6,i8/), (/2,2,2/) )
         brr3d%r = reshape ( (/r1,r3,r5,r7,r2,r4,r6,r8/), (/2,2,2/) )
         xrr3d%i = arr3d%i * brr3d%i
         xrr3d%r = arr3d%r * brr3d%r
         yrr3d%i = arr3d%i + brr3d%i
         yrr3d%r = arr3d%r + brr3d%r

         res3d = fnc ( reshape ( (/dt(i1,r1),dt(i2,r2),dt(i3,r3),             &
     &                             dt(i4,r4),dt(i5,r5),dt(i6,r6),             &
     &                             dt(i7,r7),dt(i8,r8)/),                     &
     &                           (/2,2,2/) ),                                 &
     &                 reshape ( (/dt(i1,r1),dt(i3,r3),dt(i5,r5),             &
     &                             dt(i7,r7),dt(i2,r2),dt(i4,r4),             &
     &                             dt(i6,r6),dt(i8,r8)/),                     &
     &                           (/2,2,2/) ) )
         if (any(res3d%i .ne. xrr3d%i)) error stop 113
         if (any(res3d%r .ne. xrr3d%r)) error stop 113
         print '(8(I8,F8.1))', res3d

         res3d = ent ( reshape ( (/dt(i1,r1),dt(i2,r2),dt(i3,r3),             &
     &                             dt(i4,r4),dt(i5,r5),dt(i6,r6),             &
     &                             dt(i7,r7),dt(i8,r8)/),                     &
     &                           (/2,2,2/) ),                                 &
     &                 reshape ( (/dt(i1,r1),dt(i3,r3),dt(i5,r5),             &
     &                             dt(i7,r7),dt(i2,r2),dt(i4,r4),             &
     &                             dt(i6,r6),dt(i8,r8)/),                     &
     &                           (/2,2,2/) ) )
         if (any(res3d%i .ne. yrr3d%i)) error stop 114
         if (any(res3d%r .ne. yrr3d%r)) error stop 114
         print '(8(I8,F8.1))', res3d

      end program elem07e


!--------------------------------------------------------------------!
! External elemental function with ENTRY and derived type arguments. !
!--------------------------------------------------------------------!

      elemental function fnc (a, b)
         type dt
            sequence
            integer i
            real    r
         end type dt
         type(dt), value :: a
         type(dt), value :: b
         type(dt)             :: fnc
         type(dt)             :: ent
         fnc%i = a%i * b%i
         fnc%r = a%r * b%r
         return
      entry ent (a, b)
         ent%i = a%i + b%i
         ent%r = a%r + b%r
         return
      end function fnc
