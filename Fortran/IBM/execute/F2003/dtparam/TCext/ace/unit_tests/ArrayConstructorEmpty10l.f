! GM DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/ace/unit_tests/ArrayConstructorEmpty10.f

!***********************************************************************
!* =====================================================================
!*
!*                               ArrayConstructorEmpty10 by Rob James)
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
      call sub(0)
      contains
        subroutine sub(n)
          type(t(20,4)) i(n)
          i = [ t(20,4) :: ]
        end subroutine
      end
