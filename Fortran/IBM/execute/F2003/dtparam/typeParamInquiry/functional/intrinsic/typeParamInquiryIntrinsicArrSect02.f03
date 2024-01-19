!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 6 2008
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
!* 3. ARRAY SECTION AND SUBSTRING
!* 4. DEFECT 354698,354606
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   integer(8) :: i(5)
   real(16)   :: r(len('abc'//'efg'))
   complex    :: x(3:6)
   logical    :: l(1:-1)
   character(len=3)         :: c1(4)="abc"


   contains

      function getchar1(c1,c2)
         character(*),intent(in) :: c1(:),c2(:)
         character(c1%len+c2%len):: getchar1(size(c1)+size(c2))
         getchar1=c1(1)//c2(1)
      end function

      function getchar2(c1,c2)
         character(*),intent(in) :: c1(:),c2(:)
         character(2*c1%len+2*c2%len):: getchar2(2*size(c1)+2*size(c2))
         getchar2=c1(1)//c2(1)//c1(1)//c2(1)
      end function

end module

program typeParamInquiryIntrinsicArrSect02
    use m
    implicit none

    integer :: k
    character(:),allocatable :: c2(:)

    if(i(3:4)%kind /=kind(i(3:4)) .or. i(3:4)%kind /= 8)      error stop 10_4
    if(r(4:6)%kind /=kind(r(4:6)) .or. r(4:6)%kind /= 16)     error stop 11_4
    if(x(3:0)%kind /=kind(x(3:0)) .or. x(3:0)%kind /= 4)      error stop 12_4
    if(l(1:-1)%kind /=kind(l(1:-1)) .or. l(1:-1)%kind /= 4)   error stop 13_4

    if(any(c1(2:3)(1:2) /= "ab") )                            error stop 14_4

    if(len(c1(2:3)(1:2)) /= 2)                               error stop 15_4
    if(c1(2:3)(1:2)%kind /= kind(c1(2:3)(1:2)) .or. &
       c1(2:3)(1:2)%kind /= 1)                                error stop 16_4

     if(any(c1(2:3)(1:-2) /=''))                              error stop 17_4

     if(len(c1(2:3)(1:-2)) /= 0)                              error stop 18_4

     if(any(c1(1:-1)(1:3) /='') )                             error stop 19_4

     if(c1(1:-1)(1:3)%len /= len(c1(1:-1)(1:3)) .or. &
         c1(1:-1)(1:3)%len /= 3)                              error stop 20_4

      allocate(c2( size( getchar1(c1(1:1)(2:3),c1(2:3)(1:1)) ) ), &
             source= getchar1(c1(1:1)(2:3),c1(2:3)(1:1)) )

      if(any(c2 /= 'bca' ))                                   error stop 21_4
      if(c2%len /=len(c2) .or. c2%len /= 3)                   error stop 22_4
      if(c2%kind /= kind(c2) .or. c2%kind /= 1)               error stop 23_4
      if(size(c2) /=3)                                        error stop 24_4
      if(len(c2(2:2)) /= 3)    error stop 25_4

      deallocate(c2)
      allocate(c2( size( getchar2(c1(1:1)(2:3),c1(2:3)(1:1)) ) ), &
             source= getchar2(c1(1:1)(2:3),c1(2:3)(1:1)) )

      if(any(c2 /= 'bcabca' ))                                error stop 26_4
      if(c2%len /=len(c2) .or. c2%len /= 6)                   error stop 27_4
      if(c2%kind /= kind(c2) .or. c2%kind /= 1)               error stop 28_4
      if(size(c2) /=6)                                        error stop 29_4

      if(c2(2:2)%len /= 6)   error stop 30_4

end

