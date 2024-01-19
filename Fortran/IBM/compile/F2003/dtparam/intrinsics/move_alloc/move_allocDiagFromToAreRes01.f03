!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 6 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
      integer,kind :: k
      integer,len  :: l
      integer(k)   :: i(l)
   end type

   contains

     function fun1(dt)
        type(dtp(2,*)),intent(in) :: dt
        type(dtp(2,:)),allocatable :: fun1

        fun1=dt
     end function
end module

program move_allocDiagFromToAreRes01

   use m
   implicit none

   type(dtp(2,4)),allocatable :: from1
   type(dtp(2,:)),allocatable :: to1

   allocate(dtp(2,4) :: from1)
   from1%i=[-1,-2,-3,-4]

   call move_alloc(fun1(from1),fun1(from1))
   call move_alloc(fun1(to1),fun1(to1))
   call move_alloc(fun1(to1),to1)
   call move_alloc(from1,fun1(to1))

end program
