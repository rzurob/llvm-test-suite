! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/mv_Alloc/typCompatible/realkind4.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of type real
!*                               FROM is component of pointer to a DT
!*                               TO is function name
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m


      type base(n1,k1)    ! (20,4)
          integer, kind         :: k1
          integer, len          :: n1
          real(k1), allocatable :: r2(:,:)
      end type

      type(base(20,4)), target :: trg
      contains

          function func(prg)

              type(base(:,4)), pointer :: prg
              real func(:,:)
              allocatable  func

              prg => trg
              call move_alloc(prg%r2, func)
	      if ( .not. allocated(func)) error stop 25
          end function

end module

use m

    type(base(:,4)), pointer :: p
    allocate(trg%r2(-3:-1,2:4),source=reshape((/(real(i,4), i=1,9 ) /), &
                       (/3,3/) ) )

    write (6, 100) func(p)
100 format ( f10.6 )
    if ( allocated(p%r2)) error stop 23

    end
