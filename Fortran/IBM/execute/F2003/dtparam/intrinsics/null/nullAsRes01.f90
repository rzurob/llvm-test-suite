!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullAsRes01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 26 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD]) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.88 
!* 2. NULL([MOLD]) AS FUNCTION RESULT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k)
      integer,kind :: k
      integer(k)   :: i=2
   end type

   contains

     type(dtp(2)) function nullRes1(dt)
         type(dtp(2)),pointer :: dt
         pointer :: nullRes1

         nullRes1=>null(dt)
     end function

     type(dtp(2)) function nullRes3()
         pointer :: nullRes3(:)

         nullRes3=>null()
     end function
     
end module

program nullAsRes01
   use m
   implicit none

   type(dtp(2)),pointer :: dtp1=>null()
   type(dtp(4)),pointer :: dtp2=>null() 
   type(dtp(2)),pointer :: dtp3(:)=>null()
   type(dtp(4)),pointer :: dtp4(:)=>null()

   type(dtp(2)),target  :: tar1=dtp(2)(i=11)
   type(dtp(4)),target  :: tar2=dtp(4)(i=12)
   type(dtp(2)),target  :: tar3(2)=[dtp(2)(-1),dtp(2)(-2)]
   type(dtp(4)),target  :: tar4(2)=[dtp(4)(-3),dtp(4)(-4)]


   if(associated(dtp1))                         error stop 10_4
   if(associated(dtp2))                         error stop 11_4
   if(associated(dtp3))                         error stop 12_4
   if(associated(dtp4))                         error stop 13_4
 
   dtp1=>tar1
   dtp2=>tar2
   dtp3=>tar3
   dtp4=>tar4

   if(.not. associated(dtp1,tar1))              error stop 14_4
   if(.not. associated(dtp2,tar2))              error stop 15_4
   if(.not. associated(dtp3,tar3))              error stop 16_4
   if(.not. associated(dtp4,tar4))              error stop 17_4

   if(dtp1%k /= 2)                              error stop 18_4
   if(dtp1%i /= 11)                             error stop 19_4
   if(dtp2%k /= 4)                              error stop 20_4
   if(dtp2%i /= 12)                             error stop 21_4
   if(dtp3%k /= 2)                              error stop 22_4
   if(dtp4%k /= 4)                              error stop 23_4
   if(any(dtp3%i /= [ -1,-2]))                  error stop 24_4
   if(any(dtp4%i /= [-3,-4]))                   error stop 25_4

   dtp1=>nullRes1(dtp1)

   dtp2=>nullRes2()

   dtp3=>nullRes3()

   dtp4=>nullRes4(dtp4)

   if(associated(dtp1))                         error stop 26_4
   if(associated(dtp2))                         error stop 27_4
   if(associated(dtp3))                         error stop 28_4
   if(associated(dtp4))                         error stop 29_4

   dtp1=>tar1
   dtp2=>tar2
   dtp3=>tar3
   dtp4=>tar4

   if(.not. associated(dtp1,tar1))              error stop 30_4
   if(.not. associated(dtp2,tar2))              error stop 31_4
   if(.not. associated(dtp3,tar3))              error stop 32_4
   if(.not. associated(dtp4,tar4))              error stop 33_4   


   contains

     type(dtp(4)) function nullRes2()

         pointer :: nullRes2
         nullRes2=>null()
 
     end function

     type(dtp(4)) function nullRes4(dt)
         type(dtp(4)),pointer :: dt(:)
         pointer :: nullRes4(:)
         nullRes4=>null(dt)

     end function

end program

