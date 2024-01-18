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
!*  TEST CASE TITLE            : ArrayConstructorEmpty11.f
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : 09/14/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
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
      if (.not.allocated(i)) stop 1
      if (size(i) /= 0) stop 2
      end
