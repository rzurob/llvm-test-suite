! GM DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/ace/unit_tests/ArrayConstructorEmpty11.f

!***********************************************************************
!* =====================================================================
!*
!*                               ArrayConstructorEmpty11 by Rob James)
!*  DATE                       : 2008-01-29 (original: 09/14/2006)
!*
!*  DESCRIPTION                : Testing empty array constructors.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      @process check
      type t(l1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: l1
        integer(k1)      i
      end type
      type, extends(t) :: t2    ! (20,4)
        integer(k1) j
      end type
      type(t2(:,4)), allocatable :: i(:)
      i = [ t2(20,4) :: ]
      if (.not.allocated(i)) error stop 1
      if (size(i) /= 0) error stop 2
      end
