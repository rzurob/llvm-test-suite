!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 9 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK)
!* 3. TSOURCE,FSOURCE ARE SCALAR DERIVED TYPE
!* 4. DERIVED TYPE HAS DIFFERENT SCALAR CHARACTER COMPONENT
!* 5. USE INTRINSIC ASSIGNMENT
!* 5. DEFECT 355924 355926 355942
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l)
     integer(8),len  :: l=4

     character(2*l) :: c1="xlftest"
     character(:),allocatable :: c2
     character(:),pointer :: c3=>null()
     character(2*l),pointer :: c4=>null()

  end type

  contains
     function getDT1(dt)
        class(A(*)),intent(in) :: dt
        type(A(:)),allocatable :: getDT1
        print *,dt%l
        getDT1=merge(dt,dt,.true.)
        print *,getDT1%l
     end function
end module

program mergeCharComp01
   use m
   implicit none


   type(A(4)),target :: a1
   type(A(:)),allocatable :: a2
   type(A(:)),pointer :: a3=>null()
   type(A(:)),pointer :: a4=>null()

   a2=merge(a1,a1,8 > 4)

   if(a2%l /= 4)                                      error stop 10_4
   if(a2%c1%len /= 8)                                 error stop 11_4
   if(a2%c1 /= "xlftest")                             error stop 12_4
   if(allocated(a2%c2))                               error stop 13_4
   if(associated(a2%c3))                              error stop 14_4
   if(associated(a2%c4))                              error stop 15_4
   if(a2%c4%len /= 8)                                 error stop 16_4

   a1%c1(4:6)= a1%c1(1:3)
   a1%c2 = "Hello IBM"
   a1%c3=>a1%c2(1:5)
   allocate(a1%c4,source="Training"(1:5))

   a2=merge(a1,a1,.false.)

   if(a2%l /= 4)                                      error stop 17_4
   if(a2%c1%len /= 8)                                 error stop 18_4
   if(a2%c1 /= "xlfxlft")                             error stop 19_4
   if(.not. allocated(a2%c2))                         error stop 20_4
   if(a2%c2 /= "Hello IBM")                           error stop 21_4
   if(.not. associated(a2%c3))                        error stop 22_4
   if(a2%c3 /= "Hello")                               error stop 23_4
   if(.not. associated(a2%c4))                        error stop 24_4
   if(a2%c4 /= "Train")                               error stop 25_4
   if(a2%c4%len /= 8)                                 error stop 26_4


   allocate(a3,source=merge(a1,a1,(a1%l .eq. a1%l)) )

   if(a3%l /= 4)                                      error stop 27_4
   if(a3%c1%len /= 8)                                 error stop 28_4
   if(a3%c1 /= "xlfxlft")                             error stop 29_4
   if(.not. allocated(a3%c2))                         error stop 30_4
   if(a3%c2 /= "Hello IBM")                           error stop 31_4
   if(.not. associated(a3%c3))                        error stop 32_4
   if(a3%c3 /= "Hello")                               error stop 33_4
   if(.not. associated(a3%c4))                        error stop 34_4
   if(a3%c4 /= "Train")                               error stop 35_4
   if(a3%c4%len /= 8)                                 error stop 36_4

   a2=getDT1(a1)
   if(a2%l /= 4)                                      error stop 37_4
   if(a2%c1%len /= 8)                                 error stop 38_4
   if(a2%c1 /= "xlfxlft")                             error stop 39_4
   if(.not. allocated(a2%c2))                         error stop 40_4
   if(a2%c2 /= "Hello IBM")                           error stop 41_4
   if(.not. associated(a2%c3))                        error stop 42_4
   if(a2%c3 /= "Hello")                               error stop 43_4
   if(.not. associated(a2%c4))                        error stop 44_4
   if(a2%c4 /= "Train")                               error stop 45_4
   if(a2%c4%len /= 8)                                 error stop 46_4

end program
