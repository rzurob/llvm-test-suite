! GB DTP extension using:
! ftcx_dtp -qdeferredlp /tstdev/F2003/mv_Alloc/typCompatible/mixunlmtpolyDT2.f
! opt variations: -qck -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of an nonpoly DT
!*                               TO is of unlmited poly
!*                               both are dummy args
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type  :: base(k1,n1)    ! (8,2)
          integer, kind :: k1
          integer, len  :: n1
          integer(k1)   :: id
          character(n1) :: name
      end type

      contains
          subroutine sub(a, b)
              type(base(8,*)), allocatable :: a(:,:)
              class(*), allocatable  :: b(:,:)

              call move_alloc(a, b)
          end subroutine

end module

use m
      integer i
      type(base(8,2)), allocatable :: a(:,:)
      class(*), allocatable :: b(:,:)
      allocate(a(5,2), source=reshape((/ (base(8,2)(i, char(i+70)), i=1,10) /) , &
           (/5, 2/) ))
      call sub(a, b)

      if ( .not. allocated(b)) error stop 21
      if ( allocated (a)) error stop 23
      select type(b)
          type is (base(8,*))
              print *, ( (/ ( b(i,1)%id, i = 1,5) /) )
              print *, ( (/ ( b(i,1)%name, i = 1,5) /) )
              print *, ( (/ ( b(i,2)%id, i = 1,5) /) )
              print *, ( (/ ( b(i,2)%name, i = 1,5) /) )
          class default
             stop 51
      end select

      end
