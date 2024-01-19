!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 7 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1.IF DUMMY AERGUMENT HAS THE TARGET ATTRIBUTE AND THE CORRESPONDING ACTUAL ARGUMENT DOES NOT HAVE THE TARGET ATTRIBUTE OR IS ARRAY SECTION WITH VECTOR SUBSCRIPT,ANY POINTER ASSOCIATE WITH DUMMY ARGUMENT BECOME UNDEFINED WHEN EXECUTION OF THE PROCEDURE COMPLETES.
!* 2. VERIFY LOCAL POINTER WHICH IS ASSOCIATED WITH DUMMY ARGUMENT DURING PROCEDURE EXECUTION
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l1)
     integer,len   :: l1
     character(l1) :: c1
   end type
end module

program dummyArgDeferNonPolyTarget05
  use m
  implicit none

  interface

  subroutine check1(arg)
   import
   type(dtp(:)),target,allocatable :: arg
  end subroutine

  subroutine check2(arg)
   import
   type(dtp(:)),target,allocatable :: arg(:)
  end subroutine

  end interface

  type(dtp(:)),allocatable :: dtp1,dtp2(:)

  allocate(dtp1,source=dtp(3)("xlf"))

  allocate(dtp2(-2:-1),source= &
            [dtp(3)("xlf"),dtp(3)("xlc")] )

  call check1(dtp1)

  if(dtp1%c1 /= "xlc")                           error stop 14_4

  call check2(dtp2)

  if(dtp2(-2)%c1 /= "123")                       error stop 23_4
  if(dtp2(-1)%c1 /= "456")                       error stop 24_4

end program

subroutine check1(arg)
   use m
   type(dtp(:)),target,allocatable :: arg
   type(dtp(:)),pointer  :: temp1,temp2

   temp1=>arg

   allocate(temp2,source=arg)

   if(temp1%l1 /= 3)                              error stop 10_4
   if(temp1%c1 /= "xlf")                          error stop 11_4
   if(temp2%l1 /= 3)                              error stop 12_4
   if(temp2%c1 /= "xlf")                          error stop 13_4

   nullify(temp1,temp2)

   arg%c1 = "xlc"

end subroutine

subroutine check2(arg)
   use m
   type(dtp(:)),target,allocatable :: arg(:)
   type(dtp(:)),pointer  :: temp1(:),temp2(:)

   temp1=>arg

   temp2=>arg

   if(temp1%l1 /= 3)                              error stop 15_4
   if(any(temp1%c1 /= ["xlf","xlc"]))             error stop 16_4
   if(temp2%l1 /= 3)                              error stop 17_4
   if(any(temp2%c1 /= ["xlf","xlc"]))             error stop 18_4
   if(lbound(temp1,1) /= -2)                      error stop 19_4
   if(ubound(temp1,1) /= -1)                      error stop 20_4
   if(lbound(temp2,1) /= -2)                      error stop 21_4
   if(ubound(temp2,1) /= -1)                      error stop 22_4

   nullify(temp1,temp2)

   arg(-2)%c1="123"
   arg(-1)%c1="456"

end subroutine
