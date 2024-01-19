!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 25 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD])
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.88
!* 2. NULL([MOLD])
!* 3. NULLIFY ALLOCATABLE AND POINTER COMPONENT THROUGH STRUCTURE CONSTRUCTOR
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type inner(l)
    integer,len :: l
    character(l),allocatable :: c1
    character(l),pointer     :: c2
  end type

  type outter(k)
    integer,kind ::k
    type(inner(3)),allocatable :: inn1(:)
    type(inner(3)),pointer     :: inn2(:)
  end type
end module

program nullStructConstruct01
   use m
   implicit none


   type(inner(3)),target :: inner1(2)=[inner(3)(null(),null()), &
                                      inner(3)(null(),null())]

   type(outter(4)):: out=outter(4)(null(),null())

   if(allocated(inner1(1)%c1))                  error stop 10_4
   if(allocated(inner1(2)%c1))                  error stop 11_4
   if(associated(inner1(1)%c2))                 error stop 12_4
   if(associated(inner1(2)%c2))                 error stop 13_4

   if(allocated(out%inn1))                      error stop 14_4
   if(associated(out%inn2))                     error stop 15_4

   inner1(1)%c1="123"
   allocate(inner1(1)%c2,source="000")

   inner1(2)%c1="456"
   allocate(inner1(2)%c2,source="111")

   if(.not. allocated(inner1(1)%c1))            error stop 16_4
   if(.not. allocated(inner1(2)%c1))            error stop 17_4
   if(.not. associated(inner1(1)%c2))           error stop 18_4
   if(.not. associated(inner1(2)%c2))           error stop 19_4

   out%inn1=inner1
   out%inn2=>inner1

   if(.not. allocated(out%inn1))                error stop 20_4
   if(.not. associated(out%inn2))               error stop 21_4

   if(out%k /= 4)                               error stop 22_4
   if(out%inn1%l /= 3)                          error stop 23_4
   if(out%inn2%l /= 3)                          error stop 24_4
   if(out%inn1(1)%c1 /= "123")                  error stop 25_4
   if(out%inn1(2)%c1 /= "456")                  error stop 26_4
   if(out%inn2(1)%c1 /= "123")                  error stop 27_4
   if(out%inn2(2)%c1 /= "456")                  error stop 28_4

   if(out%inn1(1)%c2 /= "000")                  error stop 29_4
   if(out%inn1(2)%c2 /= "111")                  error stop 30_4
   if(out%inn2(1)%c2 /= "000")                  error stop 31_4
   if(out%inn2(2)%c2 /= "111")                  error stop 32_4

   inner1=inner(3)(null(),null())

   if(allocated(inner1(1)%c1))                  error stop 33_4
   if(allocated(inner1(2)%c1))                  error stop 34_4
   if(associated(inner1(1)%c2))                 error stop 35_4
   if(associated(inner1(2)%c2))                 error stop 36_4

   out=outter(4)(null(),null())

   if(allocated(out%inn1))                      error stop 37_4
   if(associated(out%inn2))                     error stop 38_4


end program
