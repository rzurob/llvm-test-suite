! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/14/2006
!*
!*  DESCRIPTION                : Testing empty array constructors.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      @process check
      type t
        integer i
      end type
      type, extends(t) :: t2
        integer j
      end type
      type(t2), allocatable :: i(:)
      i = [ t2 :: ]
      if (.not.allocated(i)) error stop 1
      if (size(i) /= 0) error stop 2
      end
