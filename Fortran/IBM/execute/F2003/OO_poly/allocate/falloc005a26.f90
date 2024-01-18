! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (defined operator for arrays used as
!                               the source-expr)
!*
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
      integer :: id
   end type

   type, extends(base) :: child
   end type

   interface operator(+)
      function myAdd1(a,b)
         import base, child
         type(base), intent(in), dimension(:) :: a, b
         type(base) :: myAdd1 (size(a))
      end function
   end interface

end module

program falloc005a26
   use m

   type(base), dimension(:), allocatable :: c1, c2, c3, c4

   allocate(c1(2), source=(/ base(3), base(3) /) )
   allocate(c2(2), source=(/ base(1), base(1) /)  )

   allocate(c3(2), source=myAdd1(c1,c2) )

   if (any (c3%id /= 4)) error stop 1_4

   deallocate(c3)

   allocate(c3(2), source=c1+c2)

   if (any (c3%id /= 4)) error stop 2_4
end program


function myAdd1(a,b)
   use m, only: base, child
   type(base), intent(in), dimension(:) :: a, b
   type(base):: myAdd1(size(a))

   if ( size(a) .eq. size(b) ) then
      myAdd1%id = a%id + b%id
   else
      error stop 3_4
   end if
end function

