!*  ===================================================================
!*
!*  TEST CASE NAME             : allocate001l
!*
!*  DATE                       : 2007-09-09 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: allocate statement with source= a function return
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c
      contains
         procedure, pass :: write
   end type

   type, extends(base) :: child
      integer(4) :: i
   end type

contains

   class(base(3)) function write(dtv) ! tcx: (3)
      class(base(*)), intent(in) :: dtv ! tcx: (*)
      allocatable :: write
      allocate ( write, source = dtv )
      print *, 'hi'
   end function

end module

program allocate001l
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   type(base(3))               :: b2 = base(3)('IBM') ! tcx: (3) ! tcx: (3)

   allocate ( b1, source = b2%write()  ) !<- writes b2

end program



! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 5 changes
