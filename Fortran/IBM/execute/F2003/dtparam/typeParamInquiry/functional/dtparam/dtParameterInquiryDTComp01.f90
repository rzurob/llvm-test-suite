!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY
!* 3. INQUIRY TYPE PARAMETER OF DERIVED TYPE COMPONENT
!* 4. DT COMPONENT DOESN'T HAVE TYPE PARAMETER
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type B
      character(:),allocatable :: c1(:)
      character(:),pointer     :: c2(:)=>null()
   end type

   type :: A(k,l)
     integer,kind :: k=2
     integer,len  :: l=3

     type(B) :: b
   end type

end module

  program dtParameterInquiryDTComp01
  use m
  implicit none


  type(A(2,3)) :: a1
  type(A(4,:)),pointer :: a2=>null()

  allocate(character(2*a1%l) :: a1%b%c1(a1%k+a1%l))
  a1%b%c1="xlftest"
  allocate(character(3*a1%l) :: a1%b%c2(3*a1%l))
  a1%b%c2="fortran team"

  associate(x=>a1%b)

     if(x%c1%len /= len(x%c1) .or. x%c1%len /= 7)          error stop 10_4
     if(x%c1%kind /= kind(x%c1) .or. x%c1%kind /= 1)       error stop 11_4
     if(ubound(x%c1,1) /= 5)                               error stop 12_4
     if(any(x%c1 /= 'xlftest'))                            error stop 13_4

     if(x%c2%len /= len(x%c2) .or. x%c2%len /= 9)          error stop 13_4
     if(x%c2%kind /= kind(x%c2) .or. x%c2%kind /= 1)       error stop 14_4
     if(ubound(x%c2,1) /= 9)                               error stop 15_4
     if(any(x%c2 /= 'fortran t'))                          error stop 16_4

  end associate

  allocate(A(4,5) :: a2)

  a2%b%c1=a1%b%c1(1:4)(1:3)
  a2%b%c2=>a1%b%c2(2:3)(1:7)

  associate(x=>a2%b)
     if(x%c1%len /= len(x%c1) .or. x%c1%len /= 3)          error stop 17_4
     if(x%c1%kind /= kind(x%c1) .or. x%c1%kind /= 1)       error stop 18_4
     if(ubound(x%c1,1) /= 4)                               error stop 19_4
     if(any(x%c1 /= 'xlf'))                                error stop 20_4

     if(x%c2%len /= len(x%c2) .or. x%c2%len /= 7)          error stop 21_4
     if(x%c2%kind /= kind(x%c2) .or. x%c2%kind /= 1)       error stop 22_4
     if(ubound(x%c2,1) /= 2)                               error stop 23_4
     if(any(x%c2 /= 'fortran'))                            error stop 24_4
  end associate

end
