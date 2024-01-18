!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: OO intrinsics: data-ref with > 2 part-ref (defect 300649)
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

   type :: base
      class(base), pointer :: next => null()
      character(3) :: c = 'xxx'
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
   end type

   type :: linkedlist
      class(base), pointer :: head
   end type

end module

program misc011
   use m
   class(linkedlist), pointer :: ll
   class(base), pointer :: dummy

   class(base), allocatable, target :: b1, b2

   allocate ( b1, source = base(c='IBM') )
   allocate ( b2, source = child(c='FTN', i=1000) )

   allocate ( ll )

   ll%head => b1
   b1%next => b2

   if ( .not. same_type_as ( ll%head%next, b2 ) )    error stop 1_4
   if ( .not. extends_type_of ( ll%head%next, b2 ) ) error stop 2_4

end program
