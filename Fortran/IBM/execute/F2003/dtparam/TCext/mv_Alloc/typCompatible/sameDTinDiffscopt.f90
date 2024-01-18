! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mv_Alloc/typCompatible/sameDTinDiffscopt.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of a derived-type
!*                               In different scopting unit, two DT with
!*                               same derived-type definiation
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      module m

          type t(k1)    ! (4)
             integer, kind :: k1
             sequence
             integer(k1)      i
          end type

          type(t(4)), allocatable :: a1

          contains
               subroutine sub

                  type t(k1)    ! (4)
                     integer, kind :: k1
                     sequence
                     integer(k1)      i
                  end type

                  type(t(4)), allocatable :: a2

                  allocate(a1, source = t(4)(10))
                  call move_alloc(a1, a2)
                  if ( .not. allocated(a2) ) stop 21
                  if ( allocated(a1) ) stop 31
                  if ( a2%i /= 10 ) stop 41
               end subroutine
      end module

      use m
         call sub()
      end
