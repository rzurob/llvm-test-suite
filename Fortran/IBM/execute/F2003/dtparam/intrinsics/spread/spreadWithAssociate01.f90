!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadWithAssociate01.f
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
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type first(k1)
     integer,kind :: k1
     integer(k1)  :: i1
  end type
  type second(l1)
     integer,len    :: l1
     character(l1)  :: c1
     type(first(8)) :: first1
  end type
end module

program spreadWithAssociate01
  use m
  implicit none

  integer :: i
  type(second(:)),allocatable :: second1
  type(second(:)),allocatable :: second2(:)
  type(second(:)),allocatable :: second3(:,:)

  second1= second(3)(c1="xlf",first1=first(8)(i1=1))
  second2= [second(3)(c1="12",first1=first(8)(i1=12)), &
            second(3)(c1="34",first1=first(8)(i1=34)) ]

  second3=reshape(second2,(/1,2/))

  call associate6 (spread(second1%c1,1,5))
  call associate7 (spread(second1%first1,1,5))
  call associate8 (spread(second1%l1,1,5))
  call associate10_15(x=spread(second1,1,5))
  call associate16_23 (spread(second2,1,5))
  call associate24_31 (spread(second2,2,5))
  call associate32_39 (spread(second3,1,5))
  call associate40_47 (spread(second3,2,5))
  call associate48_55 (spread(second3,3,5))

  contains

!  associate (x=>spread(second1%c1,1,5))
  subroutine associate6 (x)
     character(*), intent(in) :: x(:)

     if(any(x /= "xlf"))                          error stop 6_4
  end subroutine

!  associate (x=>spread(second1%first1,1,5))
  subroutine associate7 (x)
      type(first(8)), intent(in) :: x(:)
     if(any(x%i1 /= 1))                           error stop 7_4
  end subroutine

!  associate (x=>spread(second1%l1,1,5))
  subroutine associate8 (x)
     integer, intent(in) :: x(5)
     if(any(x /= 3))                              error stop 8_4
  end subroutine

!  associate (x=>spread(second1%first1%k1,1,5))
  subroutine associate9 (x)
     integer, intent(in) :: x(:)
     print *, x
     if(any(x /= 8))                              error stop 9_4
  end subroutine

!  associate(x=>spread(second1,1,5))
  subroutine associate10_15(x)
    type(second(*)), intent(in) :: x(:)
    if(size(x,1) /= 5)                           error stop 10_4
    if(x%l1 /= 3)                                error stop 11_4
    do i=1,5
      if(x(i)%c1 /= "xlf")                       error stop 12_4
      if(x(i)%c1%len /= 3)                       error stop 13_4
      if(x(i)%first1%k1 /= 8)                    error stop 14_4
      if(x(i)%first1%i1 /= 1)                    error stop 15_4
    end do
  end subroutine

!   associate (x=>spread(second2,1,5))
   subroutine associate16_23 (x)
      type(second(*)), intent(in) :: x(:,:)
      do i=1,5
        if(x(i,1)%l1 /= 3)                        error stop 16_4
        if(x(i,1)%c1 /= "12")                     error stop 17_4
        if(x(i,1)%first1%i1 /= 12)                error stop 18_4
        if(x(i,1)%first1%k1 /= 8)                 error stop 19_4

        if(x(i,2)%l1 /= 3)                        error stop 20_4
        if(x(i,2)%c1 /= "34")                     error stop 21_4
        if(x(i,2)%first1%i1 /= 34)                error stop 22_4
        if(x(i,2)%first1%k1 /= 8)                 error stop 23_4

      end do
   end subroutine

!   associate (x=>spread(second2,2,5))
   subroutine associate24_31 (x)
      type(second(*)), intent(in) :: x(:,:)
      do i=1,5
        if(x(1,i)%l1 /= 3)                        error stop 24_4
        if(x(1,i)%c1 /= "12")                     error stop 25_4
        if(x(1,i)%first1%i1 /= 12)                error stop 26_4
        if(x(1,i)%first1%k1 /= 8)                 error stop 27_4

        if(x(2,i)%l1 /= 3)                        error stop 28_4
        if(x(2,i)%c1 /= "34")                     error stop 29_4
        if(x(2,i)%first1%i1 /= 34)                error stop 30_4
        if(x(2,i)%first1%k1 /= 8)                 error stop 31_4

      end do
   end subroutine

!   associate (x=>spread(second3,1,5))
   subroutine associate32_39 (x)
      type(second(*)), intent(in) :: x(:,:,:)
      do i=1,5
        if(x(i,1,1)%l1 /= 3)                        error stop 32_4
        if(x(i,1,1)%c1 /= "12")                     error stop 33_4
        if(x(i,1,1)%first1%i1 /= 12)                error stop 34_4
        if(x(i,1,1)%first1%k1 /= 8)                 error stop 35_4

        if(x(i,1,2)%l1 /= 3)                        error stop 36_4
        if(x(i,1,2)%c1 /= "34")                     error stop 37_4
        if(x(i,1,2)%first1%i1 /= 34)                error stop 38_4
        if(x(i,1,2)%first1%k1 /= 8)                 error stop 39_4
      end do
   end subroutine

!   associate (x=>spread(second3,2,5))
   subroutine associate40_47 (x)
      type(second(*)), intent(in) :: x(:,:,:)
      do i=1,5
        if(x(1,i,1)%l1 /= 3)                        error stop 40_4
        if(x(1,i,1)%c1 /= "12")                     error stop 41_4
        if(x(1,i,1)%first1%i1 /= 12)                error stop 42_4
        if(x(1,i,1)%first1%k1 /= 8)                 error stop 43_4

        if(x(1,i,2)%l1 /= 3)                        error stop 44_4
        if(x(1,i,2)%c1 /= "34")                     error stop 45_4
        if(x(1,i,2)%first1%i1 /= 34)                error stop 46_4
        if(x(1,i,2)%first1%k1 /= 8)                 error stop 47_4
      end do
   end subroutine

!   associate (x=>spread(second3,3,5))
   subroutine associate48_55 (x)
      type(second(*)), intent(in) :: x(:,:,:)
      do i=1,5
        if(x(1,1,i)%l1 /= 3)                        error stop 48_4
        if(x(1,1,i)%c1 /= "12")                     error stop 49_4
        if(x(1,1,i)%first1%i1 /= 12)                error stop 50_4
        if(x(1,1,i)%first1%k1 /= 8)                 error stop 51_4

        if(x(1,2,i)%l1 /= 3)                        error stop 52_4
        if(x(1,2,i)%c1 /= "34")                     error stop 53_4
        if(x(1,2,i)%first1%i1 /= 34)                error stop 54_4
        if(x(1,2,i)%first1%k1 /= 8)                 error stop 55_4

      end do
   end subroutine
end program
