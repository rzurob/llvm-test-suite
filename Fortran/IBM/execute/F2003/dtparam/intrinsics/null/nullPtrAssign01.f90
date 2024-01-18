!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullPtrAssign01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 24 2008 
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
!* 2. NULL([MOLD])
!* 3. POINTER ASSIGNMENT WITH MOLD PRESENTED
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l)
      integer,len  :: l 
      character(l) :: c  
   end type
end module

program nullPtrAssign01
   use m
   implicit none

   type(dtp(3)),pointer :: dtp1
   type(dtp(3)),pointer :: dtp2 

   type(dtp(3)),target  :: tar1=dtp(3)(c="123")
   type(dtp(3)),target  :: tar2=dtp(3)(c="456")

   dtp1=>null(dtp1)
   dtp2=>null(dtp2)

   if(associated(dtp1))             error stop 10_4
   if(associated(dtp2))             error stop 11_4
   
   dtp1=>tar1
   dtp2=>tar2

   if(.not. associated(dtp1,tar1))  error stop 12_4
   if(.not. associated(dtp2,tar2))  error stop 13_4

   if(dtp1%l /= 3)                  error stop 14_4
   if(dtp1%c /= "123")              error stop 15_4
   if(dtp2%l /= 3)                  error stop 16_4
   if(dtp2%c /= "456")              error stop 17_4

   dtp2=>nulldtp(dtp1)

   if(associated(dtp2))             error stop 18_4

   dtp2=>dtp1

   if(.not. associated(dtp2))       error stop 19_4
   if(dtp2%c /= "123")              error stop 20_4

   dtp2=>nulldtp(null(dtp2))       

   if(associated(dtp2))             error stop 21_4

   dtp2=>associatedtp(dtp1)   

   if(.not. associated(dtp2))       error stop 22_4

   contains
 
     function nulldtp(dtp)

        type(dtp(*)),pointer :: dtp
        type(dtp(3)),pointer :: nulldtp 

        if(associated(dtp))   then 
            nulldtp=>null(dtp) 
        else
            nulldtp=>dtp
        endif 
     end function   
     
     function associatedtp(dtp)
        type(dtp(*)),pointer :: dtp
        type(dtp(3)),pointer :: associatedtp 

        if(associated(dtp))   then
            associatedtp=>null(dtp)
        endif

        associatedtp=>tar2 

     end function

end program
