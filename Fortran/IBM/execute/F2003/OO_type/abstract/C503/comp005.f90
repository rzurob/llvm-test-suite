! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*					  polymorphic abstract type components(default init, or private, public)
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

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
   end type

   type :: b1
      class(base), pointer :: bptr => null()
   end type

   type :: b2
      class(base), allocatable, dimension(:) :: balloc
   end type

end module

program comp005
   use m

   type(b1) :: b11
   class(b2), pointer :: b21

   type(child), target :: c1 = child(5)

   b11 = b1(c1)
   allocate(b21, source = b2((/child(3), child(4)/)) )

   if (.not. associated(b11%bptr) ) error stop 1_4
   if ( size(b21%balloc) .ne. 2)    error stop 2_4

   print *, b21%balloc%id

end program