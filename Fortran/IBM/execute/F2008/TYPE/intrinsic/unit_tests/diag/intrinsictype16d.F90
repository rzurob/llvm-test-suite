!* =================================================================== &
!*
!* DATE                       : March 10, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Intrinsic types in TYPE spec
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              BYTE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program intrinsictype16d

      TYPE ( byte ) :: b
      TYPE ( byte(4) ) :: b4
      TYPE ( byte ) bb
      TYPE ( byte(4) ) bb4

      b = 1
      b4 = 1
      bb = 1
      bb4 = 1

      end

      subroutine intrinsictype16d_func ()

        type byte
          integer :: i1,i2,i3
        end type

        TYPE ( byte ) :: b
        TYPE ( byte(4) ) :: b4
        TYPE ( byte ) bb
        TYPE ( byte(4) ) bb4

        b = byte(1,1,1)
        b4 = byte(1,1,1)
        bb = byte(1,1,1)
        bb4 = byte(1,1,1)

      end subroutine
