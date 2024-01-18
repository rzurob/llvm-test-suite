!* =================================================================== &
!*
!* DATE                       : February 22, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              implied-shape arrays with
!*                              valid and invalid array specs
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module impliedshape05d_mod
      integer, parameter :: c(10,10,10) = reshape([(i,i=1,1000)],[10,10,10])

      integer, parameter :: valid1 (10,10,10) = c
      integer, parameter :: invalid1 (10,10, *) = c
      integer, parameter :: invalid2 (10, *,10) = c
      integer, parameter :: invalid3 (10, *, *) = c
      integer, parameter :: invalid4 ( *,10,10) = c
      integer, parameter :: invalid5 ( *,10, *) = c
      integer, parameter :: invalid6 ( *, *,10) = c
      integer, parameter :: valid2 ( *, *, *) = c
      end

      program impliedshape05d
      integer, parameter :: c(10,10,10) = reshape([(i,i=1,1000)],[10,10,10])

      integer, parameter   :: valid1 (10,10,10) = c
      integer, parameter   :: valid2 ( *, *, *) = c
      integer, allocatable :: valid4 (:,:,:)

      integer, parameter :: invalid1 (10,10, *) = c
      integer, parameter :: invalid2 (10, *,10) = c
      integer, parameter :: invalid3 (10, *, *) = c
      integer, parameter :: invalid4 ( *,10,10) = c
      integer, parameter :: invalid5 ( *,10, *) = c
      integer, parameter :: invalid6 ( *, *,10) = c

      integer, parameter :: invalid11 (:,:, *) = c
      integer, parameter :: invalid12 (:, *,:) = c
      integer, parameter :: invalid13 (:, *, *) = c
      integer, parameter :: invalid14 ( *,:,:) = c
      integer, parameter :: invalid15 ( *,:, *) = c
      integer, parameter :: invalid16 ( *, *,:) = c
      integer, parameter :: invalid17 (:,:,:) = c

      integer, allocatable :: invalid21 (:,:,:) = c
      integer, allocatable :: invalid22 ( *, *, *) = c
      integer, allocatable :: invalid23 (:,:, *) = c
      integer, allocatable :: invalid24 (:, *,:) = c
      integer, allocatable :: invalid25 (:, *, *) = c
      integer, allocatable :: invalid26 ( *,:,:) = c
      integer, allocatable :: invalid27 ( *,:, *) = c
      integer, allocatable :: invalid28 ( *, *,:) = c

      integer, allocatable :: invalid32 ( *, *, *)
      integer, allocatable :: invalid33 (:,:, *)
      integer, allocatable :: invalid34 (:, *,:)
      integer, allocatable :: invalid35 (:, *, *)
      integer, allocatable :: invalid36 ( *,:,:)
      integer, allocatable :: invalid37 ( *,:, *)
      integer, allocatable :: invalid38 ( *, *,:)
      contains
      subroutine foo()

      integer, parameter :: valid1 (10,10,10) = c
      integer, parameter :: invalid1 (10,10, *) = c
      integer, parameter :: invalid2 (10, *,10) = c
      integer, parameter :: invalid3 (10, *, *) = c
      integer, parameter :: invalid4 ( *,10,10) = c
      integer, parameter :: invalid5 ( *,10, *) = c
      integer, parameter :: invalid6 ( *, *,10) = c
      integer, parameter :: valid2 ( *, *, *) = c

      end subroutine

      subroutine sub1(invalid31,invalid32,invalid33,invalid34, &
                  &   invalid35,invalid36,invalid37,invalid38)
      integer, parameter :: invalid31 (:,:,:) = c
      integer, parameter :: invalid32 ( *, *, *) = c
      integer, parameter :: invalid33 (:,:, *) = c
      integer, parameter :: invalid34 (:, *,:) = c
      integer, parameter :: invalid35 (:, *, *) = c
      integer, parameter :: invalid36 ( *,:,:) = c
      integer, parameter :: invalid37 ( *,:, *) = c
      integer, parameter :: invalid38 ( *, *,:) = c
      end subroutine

      subroutine sub2(invalid41,invalid42,invalid43,invalid44, &
                  &   invalid45,invalid46,invalid47,invalid48)
      integer :: invalid41 (:,:,:) = c
      integer :: invalid42 ( *, *, *) = c
      integer :: invalid43 (:,:, *) = c
      integer :: invalid44 (:, *,:) = c
      integer :: invalid45 (:, *, *) = c
      integer :: invalid46 ( *,:,:) = c
      integer :: invalid47 ( *,:, *) = c
      integer :: invalid48 ( *, *,:) = c
      end subroutine

      subroutine sub3(invalid51,invalid52,invalid53,invalid54, &
                  &   invalid55,invalid56,invalid57,invalid58)
      integer :: invalid51 (:,:,:)
      integer :: invalid52 ( *, *, *)
      integer :: invalid53 (:,:, *)
      integer :: invalid54 (:, *,:)
      integer :: invalid55 (:, *, *)
      integer :: invalid56 ( *,:,:)
      integer :: invalid57 ( *,:, *)
      integer :: invalid58 ( *, *,:)
      end subroutine
      end
end
