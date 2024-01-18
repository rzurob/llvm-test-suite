!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryAssumedTypeParam08.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 22 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
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
!* 1. TEST SECTION 6.1.3 
!* 2. TYPE PARAMETER INQUIRY
!* 3. WITHOUT COMPONENT
!* 4. ARGUMENT ASSOCIATION
!* 5. TEST FUNCTION RESULT 
!* 6. DEFECT 354117  
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(k,l)
      integer(2),kind :: k
      integer(8),len  :: l
   end type
   
   contains

     function fun1(b)    
        type(base(2,*)),intent(in) :: b 
        class(base(2*b%k,:)),allocatable :: fun1
        allocate(base(2*b%k,2*b%l) :: fun1)
     end function

     function fun2(l)
        type(base(2,:)),pointer :: fun2 
        integer(8) :: l
        allocate(base(2,l) :: fun2)
     end function
end module

  program dtParameterInquiryAssumedTypeParam08 
  use m
  implicit none
  interface
     function fun5(b)
        import 
        type(base(2,*)),intent(in) :: b
        integer(b%k) :: fun5(b%l)
     end function
   end interface 

   type(base(2,5)) :: b1
   type(base(2*b1%k,:)),allocatable  :: b2
   type(base(2,:)),pointer :: b3
   class(base(2,:)),allocatable :: b4
   character(:),allocatable :: c(:)
   integer(b1%k),allocatable :: i(:)

   allocate(b2,source=fun1(b1))
   if(b2%k /= 4)                                          error stop 10_4
   if(b2%l /= 10)                                         error stop 11_4 
   if(b2%k%kind /=kind(b2%k) .or. b2%k%kind /= 2)         error stop 12_4
   if(b2%l%kind /=kind(b2%l) .or. b2%l%kind /= 8)         error stop 13_4
 
   allocate(b3,source=fun2(b2%l))
   if(b3%k /= 2)                                           error stop 14_4
   if(b3%l /= 10)                                          error stop 15_4
   if(b3%k%kind /=kind(b3%k) .or. b3%k%kind /= 2)          error stop 16_4
   if(b3%l%kind /=kind(b3%l) .or. b3%l%kind /= 8)          error stop 17_4
 
   allocate(b4,source=fun3(b1)) 
   if(b4%k /= 2)                                           error stop 18_4
   if(b4%l /= 5)                                           error stop 19_4
   if(b4%k%kind /=kind(b4%k) .or. b4%k%kind /= 2)          error stop 20_4
   if(b4%l%kind /=kind(b4%l) .or. b4%l%kind /= 8)          error stop 21_4

    allocate(c(b1%l),source=fun4(b1))
    if(c%kind /= kind(c) .or. c%kind /= 1)                   error stop 22_4
    if(ubound(c,1) /= b1%l)                                  error stop 23_4

    allocate(i(b1%l),source=fun5(b1))
    if(any(i /= 2))                                          error stop 24_4
    if(ubound(i,1) /= 5)                                     error stop 25_4    

  contains
      function fun3(b)  ! defect 354117
        type(base(2,*)),intent(in) :: b
        type(base(b%k,:)),allocatable :: fun3
        allocate(base(b%k,b%l) :: fun3) 
      end function

      function fun4(b)
        class(base(2,*)),intent(in) :: b
        character(:),allocatable    :: fun4(:)
        allocate(character(len=b%l) :: fun4(b%l))
        fun4='ABC'
      end function 
end

  function fun5(b)
    use m
    type(base(2,*)),intent(in) :: b
    integer(2),allocatable :: fun5(:)
    allocate(integer(2) :: fun5(b%l))
    fun5=b%k 
  end function

