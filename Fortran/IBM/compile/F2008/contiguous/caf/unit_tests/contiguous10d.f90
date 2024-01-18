!*  ===================================================================
!*
!*  DATE                       : June 24, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS simply contiguous
!*                               6.5.4 & 12.5.2.7
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 CONTIGUOUS simply contig
!*                               Testing 6.5.4, 12.5.2.7 CONTIGUOUS
!*                               C1241
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program contiguous10d

        integer, save              :: ia  (9)[*]
        integer :: stride

        ! If the dummy argument is an array coarray that has the
        !  CONTIGUOUS attribute or is not of assumed shape, the
        !  corresponding actual argument shall be simply contiguous.

        stride = 1

        ! All actual arguments are simply contiguous:
        call sub_arg_coarray_a(ia)
        call sub_arg_coarray_c(ia)
        call sub_arg_coarray_d(ia)

        call sub_arg_coarray_a(ia(1:2:2))
        call sub_arg_coarray_c(ia(1:2:2))
        call sub_arg_coarray_d(ia(1:2:2))

        ! No actual arguments are simply contiguous:
        call sub_arg_coarray_a(ia([1]))
        call sub_arg_coarray_c(ia([1]))
        call sub_arg_coarray_d(ia([1]))

        call sub_arg_coarray_a(ia(::stride))
        call sub_arg_coarray_c(ia(::stride))
        call sub_arg_coarray_d(ia(::stride))

        call sub_arg_coarray_a(ia(1:3:2))
        call sub_arg_coarray_c(ia(1:3:2))
        call sub_arg_coarray_d(ia(1:3:2))

       contains
        subroutine sub_arg_coarray_a(assumed)
          integer             :: assumed        (:)[*]
        end subroutine
        subroutine sub_arg_coarray_c(contig_assumed)
          integer, contiguous :: contig_assumed (:)[*]
        end subroutine
        subroutine sub_arg_coarray_d(deferred)
          integer             :: deferred       (*)[*]
        end subroutine

      end
