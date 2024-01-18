! GM DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/ace/unit_tests/ArrayConstructorTypeSpec07.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : ArrayConstructorTypeSpec07l_dlp
!*
!*                               ArrayConstructorTypeSpec07 by Rob James)
!*  DATE                       : 2008-01-29 (original: 09/14/2006)
!*
!*  DESCRIPTION                : Testing array constructors with type
!*  specifications as actual arguments.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      type t(l1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: l1
        integer(k1)      i
      end type
      type, extends(t) :: t2(l2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: l2
        integer(k2)      j
      end type
      class(t(:,4)), allocatable :: x
      allocate(x, source=t2(20,4,20,4)(5,10))
      call sub([x])
      contains
        subroutine sub(a)
          class(t(*,4)) :: a(:)
          select type(a)
            type is (t(*,4))
              stop 1
            type is (t2(*,4,*,4))
              if (size(a) /= 1) stop 2
              if (a(1)%i /= 5) stop 3
              if (a(1)%j /= 10) stop 4
            class default
              stop 5
          end select
        end subroutine
      end
