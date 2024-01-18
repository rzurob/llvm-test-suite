!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : contiguous01d.f
!*
!*  PROGRAMMER                 : David Nichols
!*  DATE                       : June 24, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute - C530
!*
!*  DRIVER STANZA              : xlf2008
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 attribute
!*                               CONTIGUOUS
!*                               Testing 5.3.7 C530 CONTIGUOUS
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program contiguous01d

        ! C530 An entity with the CONTIGUOUS attribute shall be an array pointer
        ! or an assumed-shape array.

        ! Valid
        integer, contiguous, pointer      :: valid1(:)

        ! Invalid
        integer, target, contiguous       :: invalid1(1)
        integer, allocatable, contiguous  :: invalid2(:)
        integer, contiguous               :: invalid3
        integer, pointer, contiguous      :: invalid4
        integer, dimension(1), contiguous :: invalid5

        contains

        subroutine contiguous_sub (valid2,valid3,valid4,invalid6,invalid7)
          ! Value CONTIGUOUS attribute on dummy agrument
          ! Assumed-shape arrays
          integer, contiguous                             :: valid2 (:), valid3 (0:) 
          ! Array pointer
          integer, contiguous, pointer                    :: valid4 (:, :) 

          ! Invalid CONTIGUOUS attribute on dummy argument
          ! Automatic explicit-shape array
          integer, contiguous, dimension (0, 10)          :: invalid6 
          ! Allocatable array
          integer, contiguous, allocatable, dimension (:) :: invalid7
          ! Implied-shape array
          ! Not implemented yet. In plan for 14.1.
          ! integer, contiguous, parameter                  :: invalid8(0:*) = [1, 1]
        end subroutine

      end
