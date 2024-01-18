!*  ===================================================================
!*
!*  DATE                       : June 24, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS substrings 6.4.1.1
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 CONTIGUOUS substrings
!*                               Testing 6.4.1.1 CONTIGUOUS
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program contiguous06d

        ! 6.4.1 Substrings
        ! 1: A substring is a contiguous portion of a character string

        character(:),  pointer   :: cpac(:)
        character(10), target    :: c(10)

        contiguous               :: cpac

        integer :: one, four, ten

        one  = 1
        four = 4
        ten  = 10

        ! Full string
        cpac => c(1:10)
        cpac => c(1:10:1)
        cpac => c(1:10:2)
        cpac => c(:)
        cpac => c(1:2)

        ! Full strings again
        cpac => c(1:10)(:)
        cpac => c(1:10:1)(:)
        cpac => c(1:10:2)(:)
        cpac => c(:)(:)
        cpac => c(1:2)(:)

        ! Substring (1:2)
        cpac => c(1:10)(1:2)
        cpac => c(1:10:1)(1:2)
        cpac => c(1:10:2)(1:2)
        cpac => c(:)(1:2)
        cpac => c(1:2)(1:2)

        ! Full strings
        cpac => c(1:10)(1:10)
        cpac => c(1:10:1)(1:10)
        cpac => c(1:10:2)(1:10)
        cpac => c(:)(1:10)
        cpac => c(1:2)(1:10)
        cpac => c(min(1,4):max(1,4))(1:10)
        cpac => c(one:ten:four)(1:10)
        cpac => c(one:ten:one)(1:10)

        ! Substring (1:1)
        cpac => c(1:10)(1:1)
        cpac => c(1:10:1)(1:1)
        cpac => c(1:10:2)(1:1)
        cpac => c(:)(1:1)
        cpac => c(1:2)(1:1)

        ! Empty substring (1:0)
        cpac => c(1:10)(1:0)
        cpac => c(1:10:1)(1:0)
        cpac => c(1:10:2)(1:0)
        cpac => c(:)(1:0)
        cpac => c(1:2)(1:0)
      end
