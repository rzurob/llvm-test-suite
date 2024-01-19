! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        returns polymorphic array abstract type
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

contains

   class(base) function foo(a)
      pointer :: foo(:)
      class(base), intent(in), dimension(:) :: a
      allocate(foo(size(a)), source=a )
   end function

   function foo1(a) result (boo)
      class(base), pointer, dimension(:) :: boo
      class(base), intent(in), dimension(:) :: a
      allocate(boo(size(a)), source=a)
   end function

end module

program funcRetrn006
   use m

   class(base), pointer, dimension(:) :: c
   class(base), allocatable, dimension(:) :: b

   allocate (b(2), source = (/ child(4), child(5) /) )

   c => foo(b)
   if (c(1)%id .ne. 4) error stop 1_4
   if (c(2)%id .ne. 5) error stop 2_4
   deallocate (c)

   allocate ( c(2), source = foo1((/child(5), child(6) /)) )
   if (c(1)%id .ne. 5) error stop 3_4
   if (c(2)%id .ne. 6) error stop 4_4

end program

