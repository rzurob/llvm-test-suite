!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullInitInDeclar01.f   
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
!* 2. NULL([MOLD])
!* 3. INITIALIZATION OF DERIVED TYPE OBJECT IN DECLARATION 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k)
     integer,kind :: k
     integer(k) :: i
   end type

   contains

     function func1()
        type(dtp(2)),pointer :: dtp2=>null()
        type(dtp(2)),pointer :: func1
        if(associated(dtp2))                         error stop 10_4
        allocate(dtp2,source=dtp(2)(i=1))
        func1=>dtp2 
     end function 
end module

program nullInitInDeclar01
   use m
   implicit none

   interface
      function func2()
          import
          type(dtp(2)),pointer :: dt=>null()
          type(dtp(2)),pointer :: func2 
      end function
   end interface 

   type(dtp(2)),pointer     :: dtp1=>null()

   if(associated(dtp1))                              error stop 11_4

   dtp1=>func1()

   if(.not. associated(dtp1))                        error stop 12_4
   if(dtp1%k /= 2)                                   error stop 13_4
   if(dtp1%i /= 1)                                   error stop 14_4

   dtp1=>func2()       
   if(associated(dtp1))                              error stop 16_4

   dtp1=>null(dtp1)
   if(associated(dtp1))                              error stop 17_4

end program
 
function func2() 
    use m
    type(dtp(2)),pointer :: dt=>null() 
    type(dtp(2)),pointer :: func2

    if(associated(dt))                               error stop 15_4

    func2=>dt
end function 

