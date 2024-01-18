! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/mv_Alloc/diag/polyToFm1.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : If FROM is poly, TO must be poly.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type A(k1,n1)    ! (4,20)
          integer, kind :: k1
          integer, len  :: n1
      end type

      type(A(4,:)), allocatable :: TO
      class(A(4,:)), allocatable :: FROM

      call move_alloc(FROM, TO )

      end
