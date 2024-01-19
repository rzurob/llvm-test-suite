!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 24 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. ASSOCIATE WITH SPREAD
!*  3. RESULT OF SPREAD IS POLYMORPHIC
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
     integer,len   :: l1
     character(l1) :: c1
  end type
  type,extends(base)  :: child(l2)
     integer,len      :: l2
     character(2*l2)  :: c2
  end type
end module

program spreadWithAssociate02
  use m
  implicit none

  integer :: i

  class(base(:)),pointer :: base1=>null()
  class(base(:)),pointer :: base2(:)=>null()
  class(base(:)),pointer :: base3(:,:)=>null()

  allocate(base1,source=child(2,3)(c1="12",c2="-12"))
  allocate(base2(2:5),source= &
           [child(2,3)(c1="12",c2="-12"),child(2,3)(c1="34",c2="-34"), &
            child(2,3)(c1="56",c2="-56"),child(2,3)(c1="78",c2="-78") ] )

  allocate(base3(2,2),source=reshape(base2,(/2,2/)) )

  associate(x=>spread(base1,1,5))
    select type(x)
       type is(child(*,*))
         if(x%l1 /= 2)                                  error stop 10_4
         if(x%l2 /= 3)                                  error stop 11_4
         if(any(x%c1 /= "12"))                          error stop 12_4
         if(any(x%c2 /= "-12"))                         error stop 13_4
         if(x%c1%len /= 2)                              error stop 14_4
         if(x%c2%len /= 6)                              error stop 15_4
       class default
         error stop 100_4
    end select
  end associate

  associate(x=>spread(base2,1,5))
    select type(x)
       type is(child(*,*))
         if(x%l1 /= 2)                                  error stop 16_4
         if(x%l2 /= 3)                                  error stop 17_4
         if(x%c1%len /= 2)                              error stop 18_4
         if(x%c2%len /= 6)                              error stop 19_4
         do i=1,5
             if(x(i,1)%c1 /= "12")                      error stop 20_4
             if(x(i,1)%c2 /= "-12")                     error stop 21_4
             if(x(i,2)%c1 /= "34")                      error stop 22_4
             if(x(i,2)%c2 /= "-34")                     error stop 23_4
             if(x(i,3)%c1 /= "56")                      error stop 26_4
             if(x(i,3)%c2 /= "-56")                     error stop 27_4
             if(x(i,4)%c1 /= "78")                      error stop 30_4
             if(x(i,4)%c2 /= "-78")                     error stop 31_4
         end do
       class default
         error stop 101_4
    end select
  end associate

  associate(x=>spread(base2,2,5))
    select type(x)
       type is(child(*,*))
         if(x%l1 /= 2)                                  error stop 32_4
         if(x%l2 /= 3)                                  error stop 33_4
         if(x%c1%len /= 2)                              error stop 34_4
         if(x%c2%len /= 6)                              error stop 35_4
         do i=1,5
             if(x(1,i)%c1 /= "12")                      error stop 36_4
             if(x(1,i)%c2 /= "-12")                     error stop 37_4
             if(x(2,i)%c1 /= "34")                      error stop 38_4
             if(x(2,i)%c2 /= "-34")                     error stop 39_4
             if(x(3,i)%c1 /= "56")                      error stop 40_4
             if(x(3,i)%c2 /= "-56")                     error stop 41_4
             if(x(4,i)%c1 /= "78")                      error stop 42_4
             if(x(4,i)%c2 /= "-78")                     error stop 43_4
         end do
        class default
          error stop 102_4
    end select
  end associate

  associate(x=>spread(base3,1,5))
    select type(x)
       type is(child(*,*))
         if(x%l1 /= 2)                                  error stop 44_4
         if(x%l2 /= 3)                                  error stop 45_4
         if(x%c1%len /= 2)                              error stop 46_4
         if(x%c2%len /= 6)                              error stop 47_4
         do i=1,5
             if(x(i,1,1)%c1 /= "12")                    error stop 48_4
             if(x(i,1,1)%c2 /= "-12")                   error stop 49_4
             if(x(i,2,1)%c1 /= "34")                    error stop 50_4
             if(x(i,2,1)%c2 /= "-34")                   error stop 51_4
             if(x(i,1,2)%c1 /= "56")                    error stop 52_4
             if(x(i,1,2)%c2 /= "-56")                   error stop 53_4
             if(x(i,2,2)%c1 /= "78")                    error stop 54_4
             if(x(i,2,2)%c2 /= "-78")                   error stop 55_4
         end do
       class default
          error stop 103_4
    end select
  end associate

  associate(x=>spread(base3,2,5))
    select type(x)
       type is(child(*,*))
         if(x%l1 /= 2)                                  error stop 56_4
         if(x%l2 /= 3)                                  error stop 57_4
         if(x%c1%len /= 2)                              error stop 58_4
         if(x%c2%len /= 6)                              error stop 59_4
         do i=1,5
             if(x(1,i,1)%c1 /= "12")                    error stop 60_4
             if(x(1,i,1)%c2 /= "-12")                   error stop 61_4
             if(x(2,i,1)%c1 /= "34")                    error stop 62_4
             if(x(2,i,1)%c2 /= "-34")                   error stop 63_4
             if(x(1,i,2)%c1 /= "56")                    error stop 64_4
             if(x(1,i,2)%c2 /= "-56")                   error stop 65_4
             if(x(2,i,2)%c1 /= "78")                    error stop 66_4
             if(x(2,i,2)%c2 /= "-78")                   error stop 67_4
         end do
       class default
          error stop 104_4
    end select
  end associate

  associate(x=>spread(base3,3,5))
    select type(x)
       type is(child(*,*))
         if(x%l1 /= 2)                                  error stop 68_4
         if(x%l2 /= 3)                                  error stop 69_4
         if(x%c1%len /= 2)                              error stop 70_4
         if(x%c2%len /= 6)                              error stop 71_4
         do i=1,5
             if(x(1,1,i)%c1 /= "12")                    error stop 72_4
             if(x(1,1,i)%c2 /= "-12")                   error stop 73_4
             if(x(2,1,i)%c1 /= "34")                    error stop 74_4
             if(x(2,1,i)%c2 /= "-34")                   error stop 75_4
             if(x(1,2,i)%c1 /= "56")                    error stop 76_4
             if(x(1,2,i)%c2 /= "-56")                   error stop 77_4
             if(x(2,2,i)%c1 /= "78")                    error stop 78_4
             if(x(2,2,i)%c2 /= "-78")                   error stop 79_4
         end do
        class default
           error stop 105_4
    end select
  end associate
end program
