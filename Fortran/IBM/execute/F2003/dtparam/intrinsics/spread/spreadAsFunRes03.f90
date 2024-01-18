!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadAsFunRes03.f
!*
!*  DATE                       : Oct. 21 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. USE SPREAD AS FUNCTION RESULT,PASS SOURCE,DIM,NCOPIES AS ACTUAL ARGUMENT
!*  3. USE DIFFERENT FUNCTION (INTERAL,EXTERNAL)
!*  4. SOURCE HAS DIFFERENT DIMENSIONS
!*  5. SOURCE IS POLYMORPHIC
!*  6. DEFECT 357751
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1)
     integer(2),kind :: k1
     integer(2*k1),allocatable    :: i(:)
   end type

   type,extends(base) :: child(l1)
     integer,len :: l1
     character(l1),allocatable :: c(:)
   end type

   contains
      function getSpreadResult3(source,dim,ncopies)
          class(base(2)),intent(in) :: source(:,:)
          class(base(2)),allocatable:: getSpreadResult3(:,:,:)
          integer,intent(in) :: dim,ncopies

          if(ncopies >= 1) then
               select case(dim)
                 case(1)
                    allocate(getSpreadResult3(ncopies,   &
                             size(source,1),size(source,2) ), &
                             source=spread(source,dim,ncopies))
                 case(2)
                    allocate(getSpreadResult3(size(source,1),   &
                             ncopies,size(source,2) ), &
                             source=spread(source,dim,ncopies))
                 case(3)
                    allocate(getSpreadResult3(size(source,1),   &
                             size(source,2),ncopies), &
                             source=spread(source,dim,ncopies))
                 case default
                    error stop 200_4
               end select
             else
                error stop 201_4
          end if
      end function

end module

program spreadAsFunRes03
  use m
  implicit none

  interface
     function getSpreadResult2(source,dim,ncopies)
        import
        class(base(2)),intent(in) :: source(:)
        integer,intent(in) :: dim,ncopies
        class(base(2)),allocatable :: getSpreadResult2(:,:)
    end function
  end interface

  integer :: i

  class(base(2)),allocatable :: base1
  class(base(2)),allocatable :: base2(:)
  class(base(2)),allocatable :: base3(:,:)
  class(base(2)),allocatable :: base4(:,:,:)
  integer,target :: i1(4),i2(4),i3(4),i4(4)

  allocate(base1,source=child(2,3)([1,2,3],c=["1","2","3"]) )

  allocate(base2(5),source=getSpreadResult1(base1,1,5))

  select type(x=>base2)
    type is(child(2,*))
      do i=1,5
        if(x%k1 /= 2)                               error stop 10_4
        if(x%l1 /= 3)                               error stop 11_4
        if(any(x(i)%i /= [1,2,3]))                  error stop 12_4
        if(any(x(i)%c /= ["1","2","3"]))            error stop 13_4
        if(x(i)%i%kind /= 4)                        error stop 14_4
        if(x(i)%c%len /= 3)                         error stop 15_4
      end do
    class default
      error stop 100_4
  end select

  if(allocated(base2))  deallocate(base2)

  allocate(base2(2:5),source=[child(2,3)([1,2,3],["1","2","3"]), &
                              child(2,3)([-1,-2,-3],["-1","-2","-3"]),&
                              child(2,3)([4,5,6],["4","5","6"]), &
                              child(2,3)([-4,-5,-6],["-4","-5","-6"]) ] )

  allocate(base3(5,4),source=getSpreadResult2(base2,1,5))

  select type(x=>base3)
    type is(child(2,*))
      do i=1,5
        if(x%k1 /= 2)                               error stop 16_4
        if(x%l1 /= 3)                               error stop 17_4
        if(any(x(i,1)%i /= [1,2,3]))                error stop 18_4
        if(any(x(i,1)%c /= ["1","2","3"]))          error stop 19_4
        if(any(x(i,2)%i /= [-1,-2,-3]))             error stop 20_4
        if(any(x(i,2)%c /= ["-1","-2","-3"]))       error stop 21_4
        if(any(x(i,3)%i /= [4,5,6]))                error stop 22_4
        if(any(x(i,3)%c /= ["4","5","6"]))          error stop 23_4
        if(any(x(i,4)%i /= [-4,-5,-6]))             error stop 24_4
        if(any(x(i,4)%c /= ["-4","-5","-6"]))       error stop 25_4
        if(x(i,1)%i%kind /= 4)                      error stop 26_4
        if(x(i,1)%c%len /= 3)                       error stop 27_4
      end do
    class default
       error stop 101_4
  end select

  if(allocated(base3))  deallocate(base3)

  allocate(base3(4,5),source=getSpreadResult2(base2,2,5))

  select type(x=>base3)
    type is(child(2,*))
      do i=1,5
        if(x%k1 /= 2)                               error stop 28_4
        if(x%l1 /= 3)                               error stop 29_4
        if(any(x(1,i)%i /= [1,2,3]))                error stop 30_4
        if(any(x(1,i)%c /= ["1","2","3"]))          error stop 31_4
        if(any(x(2,i)%i /= [-1,-2,-3]))             error stop 32_4
        if(any(x(2,i)%c /= ["-1","-2","-3"]))       error stop 33_4
        if(any(x(3,i)%i /= [4,5,6]))                error stop 34_4
        if(any(x(3,i)%c /= ["4","5","6"]))          error stop 35_4
        if(any(x(4,i)%i /= [-4,-5,-6]))             error stop 36_4
        if(any(x(4,i)%c /= ["-4","-5","-6"]))       error stop 37_4
        if(x(1,i)%i%kind /= 4)                      error stop 38_4
        if(x(1,i)%c%len /= 3)                       error stop 39_4
      end do
     class default
        error stop 102_4
  end select

  if(allocated(base3))  deallocate(base3)

  allocate(base3(2,2),source=reshape(base2,(/2,2/)))

  allocate(base4(5,2,2),source=getSpreadResult3(base3,1,5))

  select type(x=>base4)
    type is(child(2,*))
      do i=1,5
        if(x%k1 /= 2)                               error stop 40_4
        if(x%l1 /= 3)                               error stop 41_4
        if(any(x(i,1,1)%i /= [1,2,3]))              error stop 42_4
        if(any(x(i,1,1)%c /= ["1","2","3"]))        error stop 43_4
        if(any(x(i,2,1)%i /= [-1,-2,-3]))           error stop 44_4
        if(any(x(i,2,1)%c /= ["-1","-2","-3"]))     error stop 45_4
        if(any(x(i,1,2)%i /= [4,5,6]))              error stop 46_4
        if(any(x(i,1,2)%c /= ["4","5","6"]))        error stop 47_4
        if(any(x(i,2,2)%i /= [-4,-5,-6]))           error stop 48_4
        if(any(x(i,2,2)%c /= ["-4","-5","-6"]))     error stop 49_4
        if(x(i,1,1)%i%kind /= 4)                    error stop 50_4
        if(x(i,1,1)%c%len /= 3)                     error stop 51_4
      end do
     class default
        error stop 103_4
  end select

  if(allocated(base4)) deallocate(base4)

  allocate(base4(2,5,2),source=getSpreadResult3(base3,2,5))

  select type(x=>base4)
    type is(child(2,*))
      do i=1,5
        if(x%k1 /= 2)                               error stop 52_4
        if(x%l1 /= 3)                               error stop 53_4
        if(any(x(1,i,1)%i /= [1,2,3]))              error stop 54_4
        if(any(x(1,i,1)%c /= ["1","2","3"]))        error stop 55_4
        if(any(x(2,i,1)%i /= [-1,-2,-3]))           error stop 56_4
        if(any(x(2,i,1)%c /= ["-1","-2","-3"]))     error stop 57_4
        if(any(x(1,i,2)%i /= [4,5,6]))              error stop 58_4
        if(any(x(1,i,2)%c /= ["4","5","6"]))        error stop 59_4
        if(any(x(2,i,2)%i /= [-4,-5,-6]))           error stop 60_4
        if(any(x(2,i,2)%c /= ["-4","-5","-6"]))     error stop 61_4
        if(x(1,i,1)%i%kind /= 4)                    error stop 62_4
        if(x(1,i,1)%c%len /= 3)                     error stop 63_4
      end do
     class default
         error stop 104_4
  end select

  if(allocated(base4)) deallocate(base4)

  allocate(base4(2,2,5),source=getSpreadResult3(base3,3,5))

  select type(x=>base4)
    type is(child(2,*))
      do i=1,5
        if(x%k1 /= 2)                               error stop 64_4
        if(x%l1 /= 3)                               error stop 65_4
        if(any(x(1,1,i)%i /= [1,2,3]))              error stop 66_4
        if(any(x(1,1,i)%c /= ["1","2","3"]))        error stop 67_4
        if(any(x(2,1,i)%i /= [-1,-2,-3]))           error stop 68_4
        if(any(x(2,1,i)%c /= ["-1","-2","-3"]))     error stop 69_4
        if(any(x(1,2,i)%i /= [4,5,6]))              error stop 70_4
        if(any(x(1,2,i)%c /= ["4","5","6"]))        error stop 71_4
        if(any(x(2,2,i)%i /= [-4,-5,-6]))           error stop 72_4
        if(any(x(2,2,i)%c /= ["-4","-5","-6"]))     error stop 73_4
        if(x(1,1,i)%i%kind /= 4)                    error stop 74_4
        if(x(1,1,i)%c%len /= 3)                     error stop 75_4
      end do
    class default
        error stop 105_4
  end select

  contains

     function getSpreadResult1(source,dim,ncopies)

          class(base(2)),intent(in)  :: source
          integer,intent(in) :: dim,ncopies
          class(base(2)),allocatable :: getSpreadResult1(:)
          if(ncopies >= 1) then
             allocate(getSpreadResult1(ncopies), &
                     source=spread(source,dim,ncopies)  )
          else
             error stop 204_4
          end if
     end function
end program

function getSpreadResult2(source,dim,ncopies)
   use m
   class(base(2)),intent(in) :: source(:)
   integer,intent(in) :: dim,ncopies
   class(base(2)),allocatable :: getSpreadResult2(:,:)

   if(ncopies >=1 ) then
     select case(dim)
       case(1)
          allocate(getSpreadResult2(ncopies,size(source,1)), &
             source=spread(source,dim,ncopies) )
       case(2)
          allocate(getSpreadResult2(size(source,1),ncopies), &
             source=spread(source,dim,ncopies) )
       case default
          error stop 202_4
     end select
     else
       error stop 203_4
   end if
end function
