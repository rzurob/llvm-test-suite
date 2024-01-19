!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 20 2008
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
!* 3. BASIC ASSUMED LENGTH PARAMETER TEST
!* 4. DIFFERENT ACTUAL ARGUMENT
!* 5. INHERITANCE
!* 6. DEFECT 354013
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,l1)
      integer(2),kind :: k1
      integer(8),len :: l1
      character(l1) :: c1(k1:l1)
      integer(k1) :: i1(l1:k1)
   end type
   type,extends(base):: child(k2,l2)
      integer(4),kind :: k2
      integer(2),len :: l2
      character(l2) :: c2(k2:l2)
      integer(k2) :: i2(l2:k2)
   end type

   contains

     subroutine verify1(c,n)
      class(base(2,*)) :: c
      integer :: n
      print *,"TEST ",n
      select type(c)
        type is(child(2,*,4,*))
           print *,"it is child "
           if(c%k1 /= 2)                                     error stop 1_4
           if(c%k1%kind /=kind(c%k1) .or. c%k1%kind /= 2)    error stop 2_4
           if(c%l1 /= 3)                                     error stop 3_4
           if(c%l1%kind /=kind(c%l1) .or. c%l1%kind /= 8)    error stop 4_4
           if(c%k2 /= 4)                                     error stop 5_4
           if(c%k2%kind /=kind(c%k2) .or. c%k2%kind /= 4)    error stop 6_4
           if(c%l2 /= 5)                                     error stop 7_4
           if(c%l2%kind /=kind(c%l2) .or. c%l2%kind /= 2)    error stop 8_4

           if(c%c1%kind /=kind(c%c1) .or. c%c1%kind /= 1)    error stop 9_4
           if(c%c2%kind /=kind(c%c2) .or. c%c2%kind /= 1)    error stop 10_4
           if(c%i1%kind /=kind(c%i1) .or. c%i1%kind /= 2)    error stop 11_4
           if(c%i2%kind /=kind(c%i2) .or. c%i2%kind /= 4)    error stop 12_4

           if(c%c1%len /=len(c%c1) .or. c%c1%len /= 3)       error stop 13_4
           if(c%c2%len /=len(c%c2) .or. c%c2%len /= 5)       error stop 14_4

           if(lbound(c%c1,1) /= 2 .or. ubound(c%c1,1) /=3)   error stop 15_4
           if(lbound(c%c2,1) /= 4 .or. ubound(c%c2,1) /=5)   error stop 16_4
           if(lbound(c%i1,1) /= 1 .or. ubound(c%i1,1) /=0)   error stop 17_4
           if(lbound(c%i2,1) /= 1 .or. ubound(c%i2,1) /=0)   error stop 18_4

       type is(base(2,*))
           print *, "it is base"
           if(c%k1 /= 2)                                     error stop 19_4
           if(c%k1%kind /=kind(c%k1) .or. c%k1%kind /= 2)    error stop 20_4
           if(c%l1 /= 3)                                     error stop 21_4
           if(c%l1%kind /=kind(c%l1) .or. c%l1%kind /= 8)    error stop 22_4
           if(c%c1%kind /=kind(c%c1) .or. c%c1%kind /= 1)    error stop 23_4
           if(c%i1%kind /=kind(c%i1) .or. c%i1%kind /= 2)    error stop 24_4
           if(c%c1%len /=len(c%c1) .or. c%c1%len /= 3)       error stop 25_4
           if(lbound(c%c1,1) /= 2 .or. ubound(c%c1,1) /=3)   error stop 26_4
           if(lbound(c%i1,1) /= 1 .or. ubound(c%i1,1) /=0)   error stop 27_4

       class default
           error stop 28_4
     end select

    end subroutine

     subroutine verify2(c,n)
      class(*) :: c
      integer :: n
      print *,"TEST ",n
      select type(c)
        type is(child(2,*,4,*))
           print *,"it is child "
           if(c%k1 /= 2)                                     error stop 41_4
           if(c%k1%kind /=kind(c%k1) .or. c%k1%kind /= 2)    error stop 42_4
           if(c%l1 /= 3)                                     error stop 43_4
           if(c%l1%kind /=kind(c%l1) .or. c%l1%kind /= 8)    error stop 44_4
           if(c%k2 /= 4)                                     error stop 45_4
           if(c%k2%kind /=kind(c%k2) .or. c%k2%kind /= 4)    error stop 46_4
           if(c%l2 /= 5)                                     error stop 47_4
           if(c%l2%kind /=kind(c%l2) .or. c%l2%kind /= 2)    error stop 48_4

           if(c%c1%kind /=kind(c%c1) .or. c%c1%kind /= 1)    error stop 49_4
           if(c%c2%kind /=kind(c%c2) .or. c%c2%kind /= 1)    error stop 50_4
           if(c%i1%kind /=kind(c%i1) .or. c%i1%kind /= 2)    error stop 51_4
           if(c%i2%kind /=kind(c%i2) .or. c%i2%kind /= 4)    error stop 52_4

           if(c%c1%len /=len(c%c1) .or. c%c1%len /= 3)       error stop 53_4
           if(c%c2%len /=len(c%c2) .or. c%c2%len /= 5)       error stop 54_4

           if(lbound(c%c1,1) /= 2 .or. ubound(c%c1,1) /=3)   error stop 55_4
           if(lbound(c%c2,1) /= 4 .or. ubound(c%c2,1) /=5)   error stop 56_4
           if(lbound(c%i1,1) /= 1 .or. ubound(c%i1,1) /=0)   error stop 57_4
           if(lbound(c%i2,1) /= 1 .or. ubound(c%i2,1) /=0)   error stop 58_4

       type is(base(2,*))
           print *, "it is base"
           if(c%k1 /= 2)                                     error stop 59_4
           if(c%k1%kind /=kind(c%k1) .or. c%k1%kind /= 2)    error stop 60_4
           if(c%l1 /= 3)                                     error stop 61_4
           if(c%l1%kind /=kind(c%l1) .or. c%l1%kind /= 8)    error stop 62_4
           if(c%c1%kind /=kind(c%c1) .or. c%c1%kind /= 1)    error stop 63_4
           if(c%i1%kind /=kind(c%i1) .or. c%i1%kind /= 2)    error stop 64_4
           if(c%c1%len /=len(c%c1) .or. c%c1%len /= 3)       error stop 65_4
           if(lbound(c%c1,1) /= 2 .or. ubound(c%c1,1) /=3)   error stop 66_4
           if(lbound(c%i1,1) /= 1 .or. ubound(c%i1,1) /=0)   error stop 67_4

       class default
           error stop 68_4
     end select

    end subroutine

      class(base(2,:)) function getDT(c)
        class(base(2,*)) :: c
        pointer :: getDT
        select type(a=>c)
           type is(base(2,*))
              allocate(getDT,source=c)
           type is(child(2,*,4,*))
              allocate(getDT,source=c)
           class default
             error stop 69_4
        end select
      end function

end module

  program dtParameterInquiryAssumedTypeParam07
  use m
  implicit none

  type(base(2,3)),target :: b1
  type(base(2,:)),allocatable :: b2
  type(base(2,:)),pointer :: b3=>null()

  type(child(2,3,4,5)),target :: c1
  type(child(2,:,4,:)),allocatable :: c2
  type(child(2,:,4,:)),pointer :: c3=>null()

  class(base(2,:)),allocatable :: p1
  class(base(2,:)),pointer   :: p2=>null()

  class(*),allocatable :: p3
  class(*),pointer   :: p4=>null()

  allocate(c2,source=c1)
  c3=>c1

  allocate(p1,source=c1)
  p2=>c1

  call verify1(c1,1)
  call verify1(c2,2)
  call verify1(c3,3)
  call verify1(p1,4)
  call verify1(p2,5)


  allocate(b2,source=b1)
  b3=>b1
  call verify1(b1,6)
  call verify1(b2,7)
  call verify1(b3,8)

  call verify1(getDT(c1),9)
  call verify1(getDT(c2),10)
  call verify1(getDT(c3),11)
  call verify1(getDT(p1),12)
  call verify1(getDT(p2),13)
  call verify1(getDT(b1),14)
  call verify1(getDT(b2),15)
  call verify1(getDT(b3),16)

  deallocate(p1)
  allocate(p1,source=b1)
  p2=>b1

  call verify1(getDT(p1),17)
  call verify1(getDT(p2),18)

  allocate(p3,source=b1)
  call verify2(p3,19)
  p4=>c1
  call verify2(p4,20)

  deallocate(b2,c2,p1,p3)


end
