! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Allocate statement - type-spec cannot be non-poly abstract type
!*                                        abstract polymorphic entity to be allocated with extension of abstract type
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

end module

program alloc003
   use m
   class(base), allocatable    :: b1
   class(base), allocatable, dimension(:) :: b2

   class(child), allocatable :: c1
   type(child) :: c2 = child(1)

   allocate(c1, source= child(1) )

   allocate( b1, source = c1 )
   allocate( b2(2), source = (/c1,c2/) )

   if (( b1%id .ne. 1 ))  error stop 1_4
   if (( b2(1)%id .ne. 1 ) .or. ( b2(2)%id .ne. 1 )) error stop 2_4

end program