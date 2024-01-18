! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/mv_Alloc/diag/polyToFm.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : If FROM is poly, TO must be poly.
!*                               But TO must be type compatible with FROM
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type A(k1)    ! (4)
          integer, kind :: k1
      end type

      type, extends(A) :: B    ! (4)
      end type

      class(B(4)), allocatable :: TO
      class(A(4)), allocatable :: FROM

      call move_alloc(FROM, TO )

      end
