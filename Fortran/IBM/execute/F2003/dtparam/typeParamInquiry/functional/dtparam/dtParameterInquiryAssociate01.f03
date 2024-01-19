!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 18 2008
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
!* 3. USE ASSOCIATE,ASSOCIATE TO DERIVED TYPE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(m1)
     integer,len :: m1
     character(m1) :: c3(m1)
  end type

  type B(m2)
     integer,len   :: m2
     character(m2) :: c4(m2)
  end type

  type :: base(k1,l1)
      integer(2),kind    :: k1
      integer(8),len     :: l1
      character(l1+k1)   :: c1(l1)
      type(A(l1))        :: a1
  end type

   type,extends(base)    :: child(l2)
      integer,len        :: l2
      character(l1+k1)   :: c2(l2)
      type(B(l2))        :: b1
   end type
end module

program dtParameterInquiryAssociate01
  use m
  implicit none

  type(base(4,4))    :: b1=base(4,4)(c1="hello",&
                             a1=A(m1=4)(c3="xlf"))
  type(child(4,4,8))  :: c1=child(4,4,8)(c1="world",c2="fortran team", &
                        a1=A(m1=4)(c3="xlf"),b1=B(m2=8)(c4="test"))

  character(:),allocatable :: ch1(:)

  associate(b1=>b1)
    if(b1%k1 /= 4)                                            error stop 10_4
    if(b1%k1%kind /= kind(b1%k1) .or. b1%k1%kind /= 2)        error stop 11_4
    if(b1%l1 /= 4)                                            error stop 12_4
    if(b1%l1%kind /= kind(b1%l1) .or. b1%l1%kind /= 8)        error stop 13_4
    if(any(b1%c1 /= "hello"))                                 error stop 14_4
    if(b1%c1%len /= len(b1%c1) .or. b1%c1%len /= 8)           error stop 15_4
    if(ubound(b1%c1,1) /= 4)                                  error stop 16_4
    associate(a=>b1%a1)
       if(a%m1 /= 4)                                          error stop 17_4
       associate(c=>a%c3)
          if(c%len /= len(c) .or. c%len /= 4)                 error stop 18_4
          if(any(c /= "xlf"))                                 error stop 19_4
       end associate
    end associate
  end associate


  associate(c=>c1)
    if(c%k1 /= 4)                                             error stop 20_4
    if(c%k1%kind /= kind(c%k1) .or. c%k1%kind /= 2)           error stop 21_4
    if(c%l1 /= 4)                                             error stop 22_4
    if(c%l1%kind /= kind(c%l1) .or. c%l1%kind /= 8)           error stop 23_4
    if(any(c%c1 /= "world"))                                  error stop 24_4
    if(c%c1%len /= len(c%c1) .or. c%c1%len /= 8)              error stop 25_4
    if(ubound(c%c1,1) /= 4)                                   error stop 26_4
    associate(a=>c%a1)
       if(a%m1 /= 4)                                          error stop 27_4
       associate(c=>a%c3)
          if(c%len /= len(c) .or. c%len /= 4)                 error stop 28_4
          if(any(c /= "xlf"))                                 error stop 29_4
       end associate
    end associate

    if(c%l2 /= 8)                                             error stop 30_4
    if(c%l2%kind /= kind(c%l2) .or. c%l2%kind /= 4)           error stop 31_4
    if(any(c%c2 /= "fortran "))                               error stop 32_4
    if(c%c2%len /= len(c%c2) .or. c%c2%len /= 8)              error stop 33_4
    if(ubound(c%c2,1) /= 8)                                   error stop 34_4
    associate(b=>c%b1)
       if(b%m2 /= 8)                                          error stop 35_4
       associate(c=>b%c4//"123")
          if(c%len /= len(c) .or. c%len /= 11)                error stop 36_4
          if(any(c /= "test    123"))                         error stop 37_4
       end associate
    end associate
  end associate

  associate(a1=>b1%a1)
    if(a1%m1 /= 4)                                            error stop 38_4
    if(ubound(a1%c3,1) /= 4)                                  error stop 39_4
    if(any(a1%c3 /= "xlf"))                                   error stop 40_4
    allocate(character(2*len(a1%c3)) :: ch1(size(a1%c3)) )
    ch1(:)=a1%c3
  end associate

  if(ch1%len /= len(ch1) .or. ch1%len /= 8)                   error stop 41_4
  if(any(ch1 /= "xlf"))                                       error stop 42_4

end
