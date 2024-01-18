! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C573_001kl
!*
!*  DATE                       : 2007-07-23 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: C573
!*                                        A namelist group name shall not be a name made accessible by use association
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base (kb) ! kb=4
      integer, kind :: kb
      real(kb), allocatable :: i
      real(kb), pointer     :: j
   end type

   class(base(4)), allocatable :: b1 ! tcx: (4)
   namelist /nml/ b1

end module

program C573_001kl
   use m

   class(base(4)), pointer     :: b2 ! tcx: (4)
   type(base(4))               :: b3 ! tcx: (4)

   namelist /nml/ b2, b3

   integer :: stat
   character(200) :: msg

   allocate(b1, b1%i, b1%j)
   allocate(b2, b2%i, b2%j)
   allocate(b3%i, b3%j)

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 3 changes
