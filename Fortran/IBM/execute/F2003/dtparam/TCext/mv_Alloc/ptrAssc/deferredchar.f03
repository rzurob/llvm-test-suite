! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/mv_Alloc/ptrAssc/deferredchar.f
! opt variations: -qck -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : 1.TO is of character(:)
!*                               2.A pointer componenet of type character(:)
!*                                 associated with FROM
!*                               3.FROM is declared as ch(:)*12
!*                               4.test value and bound of pointer
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      character(1), allocatable, target :: ch(:)*12
      character(:), allocatable, target :: ch2(:)

      type A(k1)    ! (4)
          integer, kind :: k1
          character(:), pointer :: p(:)
      end type
end module

      use m

      class(A(4)), allocatable :: aA

      allocate(A(4)::aA)

      allocate(ch(-2:-1), source= (/ 'helloworld    ', 'youhavingfun  ' /) )

      aA%p => ch

      call move_alloc(ch, ch2)

      if ( allocated(ch) ) error stop 21
      if ( .not. allocated(ch2) ) error stop 23

      print *, associated(aA%P, ch2)
      print *, len(aA%p), aA%P
      print *, lbound(aA%p,1), ubound(aA%P,1)

      end