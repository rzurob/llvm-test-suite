! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*                                         d) OPTIONAL attribute with unlimited polymorphic dummy argument ( pointer, non-pointer, allocatable, non-allocatable )
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
      real :: rid
   end type

contains

   integer function boo(a, b, c)
      class(*), optional :: a
      class(*), optional, allocatable :: b
      class(*), optional, pointer :: c

      boo=1
      if (present (a) ) then
         boo=2
         if (present(b)) then
            boo=3
            if (present(c)) then
               boo=4
            end if
         end if
      end if

   end function

end module

program dummy011
   use m

   class(base), allocatable :: b1
   type(child), target :: c1 = child(3,3.4)
   class(*), allocatable :: u1
   class(*), pointer     :: u2

   allocate (b1, source = child(1,2.3))
   allocate (u1, source = c1)
   allocate (u2, source = b1)

   if ( boo()   .ne. 1 )     error stop 1_4
   if ( boo(b1) .ne. 2 )     error stop 2_4
   if ( boo(b1, u1) .ne. 3 ) error stop 3_4
   if ( boo(b1, u1, u2) .ne. 4 ) error stop 4_4
   if ( boo(c=u2) .ne. 1 ) error stop 5_4
   if ( boo(b=u1) .ne. 1 ) error stop 6_4
   if ( boo(u1) .ne. 2 ) error stop 7_4

end program