! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/mv_Alloc/typCompatible/sameDTpolyDT.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO is of poly DT same as FROM
!*                               FROM is a non-poly DT
!*                               component type is same as type defined
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type A(k1,n1)    ! (4,20)
          integer, kind           :: k1
          integer, len            :: n1
          class(A(k1,:)), pointer :: self => null()
      end type

      class(A(4,:)), allocatable, target ::  a1
      type(A(4,:)), allocatable ::  a2

      allocate(a2, source = A(4,20)(a1) )

      call move_alloc(a2, a1)

      if ( allocated(a2) ) error stop 21
      if ( .not. allocated(a1) ) error stop 23
      end
