!*  ===================================================================
!*
!*  DATE                       : June 24, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute - 5.3.7.1
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 attribute
!*                               CONTIGUOUS
!*                               Testing 5.3.7.1 CONTIGUOUS
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program contiguous02d

      ! 5.3.7.1 The CONTIGUOUS attribute specifies that an assumed-shape array
      ! can only be argument associated with a contiguous actual argument, or
      ! that an array pointer can only be pointer associated with a contiguous
      ! target.

        integer, pointer, contiguous      :: A(:)
        integer, pointer, contiguous      :: B(:)

        integer, target :: targ(10)
        integer, pointer :: alloc(:)

        allocate(alloc(10))

        A => B
        B => alloc
        A => B
        A => targ
        A => targ(1:2)

        call contiguous_sub(B,targ,A)

        contains

        subroutine contiguous_sub (A,B,C)
          ! Assumed-shape arrays
          integer, contiguous          :: A (:), B (0:)
          ! Array pointer
          integer, contiguous, pointer :: C (:)
        end subroutine

      end
