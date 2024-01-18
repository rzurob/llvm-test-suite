!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryDTComp02.f
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
!* 4. DERIVED TYPE COMPONENT HAS TYPE PARAMETER
!* 5. DEFECT 355181 355186 354013
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   character(len=*),parameter :: c1="xlftest"
   type B(k2,l2)
      integer,kind :: k2=4
      integer,len  :: l2=5

      integer(k2)  :: i2=k2
      class(*),pointer           :: c2=>null()
      character(:),pointer       :: c3=>null()
      character(l2)              :: c4=c1
   end type

   type :: A(k1,l1)
     integer,kind :: k1=2
     integer,len  :: l1=3

     type(B(2*k1,2*l1)) :: b
   end type

end module

  program dtParameterInquiryDTComp02
  use m
  implicit none


  type(A) :: a1
  type(A(4,:)),allocatable :: a2

  !--- test a1---!
  if(a1%k1 /= 2)                                          error stop 10_4
  if(a1%l1 /= 3)                                          error stop 11_4
  if(a1%b%k2 /= 4)                                        error stop 12_4
  if(a1%b%l2 /= 6)                                        error stop 13_4
  if(a1%b%i2 /= 4)                                        error stop 14_4
  if(a1%b%i2%kind /= kind(a1%b%i2) .or. a1%b%i2%kind /= 4)   error stop 15_4

  associate(x=>a1%b)
    allocate(character(2*x%l2) :: x%c3)
    x%c3=c1
  end associate
  if(a1%b%c3%len /= len(a1%b%c3) .or. a1%b%c3%len /= 12)  error stop 16_4
  if(a1%b%c3 /= "xlftest")                                error stop 17_4

  associate(x=>a1%b)
     x%c2=>x%c3(:)
  end associate

  select type(x=>a1%b%c2)
     type is(character(*))
        associate(y=>x//" test")
           if(y%len /= len(y) .or. y%len /= 17)            error stop 18_4
           if (y /= 'xlftest      test') stop 50
        end associate
     class default
       error stop 100_4
  end select
  if(a1%b%c4 /= "xlftes")                                 error stop 19_4
  if(a1%b%c4%len /= len(a1%b%c4) .or. a1%b%c4%len /= 6)   error stop 20_4

  !--- test a2---!
  allocate(A(4,2) :: a2)
  if(a2%k1 /= 4)                                          error stop 21_4
  if(a2%l1 /= 2)                                          error stop 22_4
  if(a2%b%k2 /= 8)                                        error stop 23_4
  if(a2%b%l2 /= 4)                                        error stop 24_4
  if(a2%b%i2 /= 8)                                        error stop 25_4
  if(a2%b%i2%kind /= kind(a2%b%i2) .or. a2%b%i2%kind /= 8)  error stop 26_4

  allocate(a2%b%c2,source=c1(1:3))
  select type(x=>a2%b%c2)
     type is(character(*))
         if(x%len /= len(x) .or. x%len /= 3)              error stop 27_4
         if(x /= "xlf")                                   error stop 28_4
     class default
         error stop 101_4
  end select
  allocate(a2%b%c3,source=a1%b%c3(1:3))
  if(a2%b%c3%len /= len(a2%b%c3) .or. a2%b%c3%len /= 3)   error stop 29_4
  if(a2%b%c3 /= "xlf")                                    error stop 30_4
  if(a2%b%c4 /= "xlft")                                   error stop 31_4
  if(a2%b%c4%len /= len(a2%b%c4) .or. a2%b%c4%len /= 4)   error stop 32_4

end
