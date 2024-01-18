!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryDTIntrinsic10.f
!*
!*  DATE                       : July 9 2008
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. USE EXTENDS
!* 4. COMPONENT IS ARRAY
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type first
      integer(2) ,dimension(3):: i=0
      character(:),allocatable :: c1(:)
   end type first
   type,extends(first) :: second
      character(:),pointer :: c2(:) => null()
   end type
   type,extends(second) :: third
      real,dimension(4) :: r=0.0
   end type

end module

program typeParamInquiryDTIntrinsic10
  use m
  implicit none

  character(len("3rd")),target :: t3="3rd"
  character(len("2nd")),target :: t2="2nd"
  class(first),pointer :: p
  integer  :: j

  type(first),target   :: dt1
  type(second),target  :: dt2
  type(third),target   :: dt3

  p=>dt3

  call sub(p)

  if(any(dt3%i  /= (/(j,j=1,size(dt3%i))/) ))           error stop 10_4
  if(dt3%i%kind /= kind(dt3%i) .or. dt3%i%kind /=2 )    error stop 11_4
  if(any(dt3%c1 /= "third" ) )   error stop 12_4
  if(dt3%c1%len /= 5 .or. len(dt3%c1) /= 5 )            error stop 13_4
  if(dt3%c1%kind /= kind(dt3%c1) .or. dt3%c1%kind /= 1) error stop 14_4
  if(any(dt3%c2 /= "3rd") )                             error stop 15_4
  if(dt3%c2%len /= 3 .or. len(dt3%c2) /= 3)             error stop 16_4
  if(dt3%c2%kind /= kind(dt3%c2) .or. dt3%c2%kind /= 1) error stop 17_4
  if(dt3%r%kind /= kind(dt3%r) .or. dt3%r%kind /= 4 )   error stop 18_4

  p=>dt2

  call sub(p)

  if(any(dt2%i /= (/(2**(j+1),j=1,size(dt2%i))/) ))     error stop 19_4
  if(dt2%i%kind /= kind(dt2%i) .or. dt2%i%kind /= 2)    error stop 20_4
  if(any(dt2%c1 /= "second"))                           error stop 21_4
  if(dt2%c1%len /= len(dt2%c1) .or. dt2%c1%len /= 6 )   error stop 22_4
  if(dt2%c1%kind /= kind(dt2%c1) .or. dt2%c1%kind /= 1) error stop 23_4
  if(any(dt2%c2 /= "2nd"))                              error stop 24_4
  if(dt2%c2%len /= 3 .or. len(dt2%c2) /= 3)             error stop 25_4
  if(dt2%c2%kind /= kind(dt2%c2) .or. dt2%c2%kind /= 1) error stop 26_4

  p=>dt1

  call sub(p)

  if(any(dt1%i  /= (/(2*j,j=1,size(dt1%i))/) ))         error stop 27_4
  if(dt1%i%kind /= kind(dt1%i) .or. dt1%i%kind /= 2)    error stop 28_4
  if(any(dt1%c1 /= "first"))                            error stop 29_4
  if(dt1%c1%len /= len(dt1%c1) .or. dt1%c1%len /= 5 )   error stop 30_4
  if(dt1%c1%kind /= kind(dt1%c1) .or. dt1%c1%kind /= 1) error stop 31_4

  contains
     subroutine sub(dt)
        class(first)  :: dt
        select type(dt)
           type is (third)
              dt%r=3.0
              dt%i=[1,2,3]
              allocate(dt%c1(2:4),source="third")
              allocate(dt%c2(2:4),source=t3)
           type is (second)
              dt%i=[4,8,16]
              allocate(dt%c1(2:4),source="second")
              allocate(dt%c2(2:4),source=t2)
           type is (first)
              dt%i=[2,4,6]
              allocate(dt%c1(2:4),source="first")
           class default
              print *,"should not come here"
        end select

     end subroutine
end
