!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ArrayConstructorTypeSpec05.f
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : 09/14/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing array constructors with type
!*                               specifications as actual arguments.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      use, intrinsic :: ieee_arithmetic

      call sub ((/real*4 :: sin(2.5), 2.0+16.5_16, 0.2_8/))
      contains
        subroutine sub(a)
          real(4) a(3)
          if (any(.not.isSameReal4(a,                                    &
     &            [sin(2.5_4), real(2.0+16.5_16, 4), real(0.2_8, 4)])))  &
     &      stop 1
        end subroutine

        ! Adapted from our standard precision_R4 to be elemental, and to handle NaN and Inf:
        elemental logical function isSameReal4(value,expected)
          real(4), intent(in) :: value, expected
          real(4) :: high, low, delta

          ! If they're both extreme - both NaN or Infinite with the same sign - return true
          if (ieee_is_nan(value) .and. ieee_is_nan(expected)             &
     &            .or. (value == expected)) then
             isSameReal4 = .true.
          else
             delta = expected * 0.00001
             high = delta + expected
             low = expected - delta
             ! This is still not perfect: we don't handle the range near Inf well:
             if (expected < 0.0E0) then
                isSameReal4 = ((value >= high) .and. (value <= low))
             else
                isSameReal4 = ((value <= high) .and. (value >= low))
             end if
          end if

        end function isSameReal4

      end
