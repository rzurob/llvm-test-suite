!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryAssociate02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : August 18 2008 
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
!* 3. USE ASSOCIATE
!* 4. ASSOCIATE TO DERIVED TYPE ARRAY
!* 5. ASSOCIATE TO TYPE PARAMETER INQUIRY
!* 6. ASSOCIATE TO COMPONENT
!* 7. DEFECT 355142
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k,l)
     integer(selected_int_kind(4)),kind :: k
     integer(selected_int_kind(2)),len :: l
     integer(k) :: i(k-1:k+1)
     character(l) :: c(l-1:l+1)
  end type
end module

program dtParameterInquiryAssociate02 
  use m
  implicit none

  type(base(2,:)),allocatable :: b1(:)

  allocate(base(2,3) :: b1(2))

  b1(1)=base(2,3)(i=[1,2,3],c=['abcd','efgh','ijkl'])
  b1(2)=base(2,3)(i=[-1,-2,-3],c=['red  ','blue ','green'])

  
  associate(x=>b1)
    if(x%k /= 2)                                  error stop 10_4
    if(x%l /= 3)                                  error stop 11_4
    associate(y=>x%k)
       if(y%kind /= kind(y) .or. y%kind /= 2)     error stop 12_4
    end associate 

    associate(y=>x%l)
       if(y%kind /= kind(y) .or. y%kind /= 1)     error stop 13_4
    end associate

    associate(y=>x(1)%i)
      if(y%kind /= kind(y) .or. y%kind /= 2)      error stop 14_4
      if(lbound(y,1) /= 1)                        error stop 15_4
      if(ubound(y,1) /= 3)                        error stop 16_4
      if(any(y /= [1,2,3]))                       error stop 17_4 
    end associate

    associate(y=>x(1)%c)
      if(y%kind /= kind(y) .or. y%kind /= 1)      error stop 18_4
      if(y%len /= len(y) .or. y%len /= 3)         error stop 19_4
      if(lbound(y,1) /= 2)                        error stop 20_4
      if(ubound(y,1) /= 4)                        error stop 21_4
      if(any(y /= ['abc','efg','ijk']))           error stop 22_4
    end associate

    associate(y=>x(2)%i)
      if(y%kind /= kind(y) .or. y%kind /= 2)      error stop 23_4
      if(lbound(y,1) /= 1)                        error stop 24_4
      if(ubound(y,1) /= 3)                        error stop 25_4
      if(any(y /= [-1,-2,-3]))                    error stop 26_4
    end associate

    associate(y=>x(2)%c)
      if(y%kind /= kind(y) .or. y%kind /= 1)      error stop 27_4
      if(y%len /= len(y) .or. y%len /= 3)         error stop 28_4
      if(lbound(y,1) /= 2)                        error stop 29_4
      if(ubound(y,1) /= 4)                        error stop 30_4
      if(any(y /= ['red','blu','gre']))           error stop 31_4
    end associate
    
  end associate      
end
