!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadSourceIsArrayDTComp02.f
!*
!*  DATE                       : Oct. 18 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.14
!*  2. SOURCE IS ARRAY AND HAS DERIVED TYPE ARRAY COMPONENTS
!*  3. DEFECT 357502
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type first(k1)
     integer,kind    :: k1
     integer(k1)     :: i1
   end type
   type second(l2)
     integer(2),len  :: l2
     character(l2)   :: c1
   end type
   type third(k3,l3)
     integer(8),kind :: k3
     integer,len     :: l3
     type(second(l3)) :: second1(2:l3+1)
     type(first(2*k3)) :: first1(0:k3)
   end type

end module

program spreadSourceIsArrayDTComp02
  use m
  implicit none

  type(third(1,2)) :: third1(2:5)=    &
    [third(1,2) (first1=[first(2)(i1=1),first(2)(i1=-1)],      &
                 second1=[second(2)(c1="+1") ,second(2)(c1="-1") ] ), &
     third(1,2) (first1=[first(2)(i1=2),first(2)(i1=-2)],      &
                 second1=[second(2)(c1="+2") ,second(2)(c1="-2") ] ), &
     third(1,2) (first1=[first(2)(i1=3),first(2)(i1=-3)],      &
                 second1=[second(2)(c1="+3") ,second(2)(c1="-3") ] ), &
     third(1,2) (first1=[first(2)(i1=4),first(2)(i1=-4)],      &
                 second1=[second(2)(c1="+4") ,second(2)(c1="-4") ] ) ]

  type(third(1,2)),pointer :: third2(:,:)

  allocate(third2(2,2),source=reshape(third1,(/2,2/)) )

  call verify1(spread(third1,1,5)) ! dim is 1
  !   spread(..) becomes ...
  !   | third1(2), third1(3), third1(4), third1(5) |
  !   | third1(2), third1(3), third1(4), third1(5) |
  !   | third1(2), third1(3), third1(4), third1(5) |
  !   | third1(2), third1(3), third1(4), third1(5) |
  !   | third1(2), third1(3), third1(4), third1(5) |


  call verify2(spread(third1,2,5)) ! dim is 2
  !   spread(..) becomes ...
  !   | third1(2), third1(2), third1(2), third1(2), third1(2) |
  !   | third1(3), third1(3), third1(3), third1(3), third1(3) |
  !   | third1(4), third1(4), third1(4), third1(4), third1(4) |
  !   | third1(5), third1(5), third1(5), third1(5), third1(5) |


  !   third2 is
  !   third2(1,1) - (first1=[first(2)(i1=1),first(2)(i1=-1)],
  !                  second1=[second(2)(c1="+1"),second(2)(c1="-1") ] )
  !
  !   third2(1,2) - (first1=[first(2)(i1=3),first(2)(i1=-3)],
  !                  second1=[second(2)(c1="+3"),second(2)(c1="-3") ] )
  !   third2(2,1) - (first1=[first(2)(i1=2),first(2)(i1=-2)],
  !                  second1=[second(2)(c1="+2"),second(2)(c1="-2") ] )
  !   third2(2,2) - (first1=[first(2)(i1=4),first(2)(i1=-4)],
  !                  second1=[second(2)(c1="+4"),second(2)(c1="-4") ] )

  call verify3(spread(third2,1,5)) ! dim is 1

  !   shape is 5,2,2
  !   dt(x,1,1) - (x is 1 - 5) -
  !               (first1=[first(2)(i1=1),first(2)(i1=-1)],
  !                second1=[second(2)(c1="+1"),second(2)(c1="-1") ] )
  !
  !   dt(x,1,2) - (x is 1 - 5) -
  !               (first1=[first(2)(i1=3),first(2)(i1=-3)],
  !                second1=[second(2)(c1="+3"),second(2)(c1="-3") ] )
  !
  !   dt(x,2,1) - (x is 1 - 5) -
  !               (first1=[first(2)(i1=2),first(2)(i1=-2)],
  !                second1=[second(2)(c1="+2"),second(2)(c1="-2") ] )
  !
  !   dt(x,2,2) - (x is 1 - 5) -
  !               (first1=[first(2)(i1=4),first(2)(i1=-4)],
  !                second1=[second(2)(c1="+4"),second(2)(c1="-4") ] )

  call verify4(spread(third2,2,5)) ! dim is 2

  !   shape is 2,5,2
  !   dt(1,x,1) - (x is 1 - 5) -
  !               (first1=[first(2)(i1=1),first(2)(i1=-1)],
  !                second1=[second(2)(c1="+1"),second(2)(c1="-1") ] )
  !
  !   dt(1,x,2) - (x is 1 - 5) -
  !               (first1=[first(2)(i1=3),first(2)(i1=-3)],
  !                second1=[second(2)(c1="+3"),second(2)(c1="-3") ] )
  !
  !   dt(2,x,1) - (x is 1 - 5) -
  !               (first1=[first(2)(i1=2),first(2)(i1=-2)],
  !                second1=[second(2)(c1="+2"),second(2)(c1="-2") ] )
  !
  !   dt(2,x,2) - (x is 1 - 5) -
  !               (first1=[first(2)(i1=4),first(2)(i1=-4)],
  !                second1=[second(2)(c1="+4"),second(2)(c1="-4") ] )

  contains

     subroutine verify1(dt)
        type(third(1,*)),intent(in) :: dt(:,:)
        integer :: i
        ! element order:
        ! dt(1,1) - third1(2) -
        !            (first1=[first(2)(i1=1),first(2)(i1=-1)],
        !             second1=[second(2)(c1="+1"),second(2)(c1="-1") ] )
        ! dt(2,1) - third1(2)
        ! dt(3,1) - third1(2)
        ! dt(4,1) - third1(2)
        ! dt(5,1) - third1(2)
        ! dt(1,2) - third1(3) -
        !           (first1=[first(2)(i1=2),first(2)(i1=-2)],
        !            second1=[second(2)(c1="+2"),second(2)(c1="-2") ] )
        ! dt(2,2) - third1(3)
        ! dt(3,2) - third1(3)
        ! dt(4,2) - third1(3)
        ! dt(5,2) - third1(3)
        ! dt(1,3) - third1(4) -
        !           (first1=[first(2)(i1=3),first(2)(i1=-3)],
        !            second1=[second(2)(c1="+3"),second(2)(c1="-3") ] )
        ! dt(2,3) - third1(4)
        ! dt(3,3) - third1(4)
        ! dt(4,3) - third1(4)
        ! dt(5,3) - third1(4)
        ! dt(1,4) - third1(5) -
        !           (first1=[first(2)(i1=4),first(2)(i1=-4)],
        !            second1=[second(2)(c1="+4"),second(2)(c1="-4") ] )
        ! dt(2,4) - third1(5)
        ! dt(3,4) - third1(5)
        ! dt(4,4) - third1(5)
        ! dt(5,4) - third1(5)

        if(dt%k3 /= 1)                             error stop 10_4
        if(dt%l3 /= 2)                             error stop 11_4
        if(size(dt,1) /= 5)                        error stop 12_4
        if(size(dt,2) /= 4)                        error stop 13_4
        do i=1,5
          if(dt(i,1)%first1%k1 /= 2)               error stop 14_4
          if(dt(i,1)%second1%l2 /= 2)              error stop 15_4
          if(lbound(dt(i,1)%first1,1) /= 0)        error stop 16_4
          if(ubound(dt(i,1)%first1,1) /= 1)        error stop 17_4
          if(lbound(dt(i,1)%second1,1) /= 2)       error stop 18_4
          if(ubound(dt(i,1)%second1,1) /= 3)       error stop 19_4

          if(any(dt(i,1)%first1%i1 /= [1,-1]))     error stop 20_4
          if(any(dt(i,2)%first1%i1 /= [2,-2]))     error stop 21_4
          if(any(dt(i,3)%first1%i1 /= [3,-3]))     error stop 22_4
          if(any(dt(i,4)%first1%i1 /= [4,-4]))     error stop 23_4

          if(any(dt(i,1)%second1%c1 /= ["+1","-1"]))       error stop 24_4
          if(any(dt(i,2)%second1%c1 /= ["+2","-2"]))       error stop 25_4
          if(any(dt(i,3)%second1%c1 /= ["+3","-3"]))       error stop 26_4
          if(any(dt(i,4)%second1%c1 /= ["+4","-4"]))       error stop 27_4
        end do
     end subroutine

     subroutine verify2(dt)
        type(third(1,*)),intent(in) :: dt(:,:)
        integer :: i

        ! element order
        ! dt(1,1) - third1(2) -
        !            (first1=[first(2)(i1=1),first(2)(i1=-1)],
        !             second1=[second(2)(c1="+1"),second(2)(c1="-1") ] )
        ! dt(2,1) - third1(3) -
        !           (first1=[first(2)(i1=2),first(2)(i1=-2)],
        !            second1=[second(2)(c1="+2"),second(2)(c1="-2") ] )
        ! dt(3,1) - third1(4) -
        !           (first1=[first(2)(i1=3),first(2)(i1=-3)],
        !            second1=[second(2)(c1="+3"),second(2)(c1="-3") ] )
        ! dt(4,1) - third1(5) -
        !           (first1=[first(2)(i1=4),first(2)(i1=-4)],
        !            second1=[second(2)(c1="+4"),second(2)(c1="-4") ] )
        ! dt(1,2) - third1(2)
        ! dt(2,2) - third1(3)
        ! dt(3,2) - third1(4)
        ! dt(4,2) - third1(5)
        ! dt(1,3) - third1(2)
        ! dt(2,3) - third1(3)
        ! dt(3,3) - third1(4)
        ! dt(4,3) - third1(5)
        ! dt(1,4) - third1(2)
        ! dt(2,4) - third1(3)
        ! dt(3,4) - third1(4)
        ! dt(4,4) - third1(5)
        ! dt(1,5) - third1(2)
        ! dt(2,5) - third1(3)
        ! dt(3,5) - third1(4)
        ! dt(4,5) - third1(5)

        if(dt%k3 /= 1)                             error stop 28_4
        if(dt%l3 /= 2)                             error stop 29_4
        if(size(dt,1) /= 4)                        error stop 30_4
        if(size(dt,2) /= 5)                        error stop 31_4
        do i=1,5
          if(dt(1,i)%first1%k1 /= 2)               error stop 32_4
          if(dt(1,i)%second1%l2 /= 2)              error stop 33_4
          if(lbound(dt(1,i)%first1,1) /= 0)        error stop 34_4
          if(ubound(dt(1,i)%first1,1) /= 1)        error stop 35_4
          if(lbound(dt(1,i)%second1,1) /= 2)       error stop 36_4
          if(ubound(dt(1,i)%second1,1) /= 3)       error stop 37_4

          if(any(dt(1,i)%first1%i1 /= [1,-1]))     error stop 38_4
          if(any(dt(2,i)%first1%i1 /= [2,-2]))     error stop 39_4
          if(any(dt(3,i)%first1%i1 /= [3,-3]))     error stop 40_4
          if(any(dt(4,i)%first1%i1 /= [4,-4]))     error stop 41_4

          if(any(dt(1,i)%second1%c1 /= ["+1","-1"]))       error stop 42_4
          if(any(dt(2,i)%second1%c1 /= ["+2","-2"]))       error stop 43_4
          if(any(dt(3,i)%second1%c1 /= ["+3","-3"]))       error stop 44_4
          if(any(dt(4,i)%second1%c1 /= ["+4","-4"]))       error stop 45_4
        end do
     end subroutine

    subroutine verify3(dt)
       type(third(1,*)),intent(in) :: dt(:,:,:)
       integer :: i
       ! element order:
       ! dt(1,1,1) - third2(1,1) -
       !            (first1=[first(2)(i1=1),first(2)(i1=-1)],
       !             second1=[second(2)(c1="+1"),second(2)(c1="-1") ] )
       ! dt(2,1,1) - third2(1,1)
       ! dt(3,1,1) - third2(1,1)
       ! dt(4,1,1) - third2(1,1)
       ! dt(5,1,1) - third2(1,1)

       ! dt(1,2,1) - third2(2,1) -
       !           (first1=[first(2)(i1=2),first(2)(i1=-2)],
       !            second1=[second(2)(c1="+2"),second(2)(c1="-2") ] )
       ! dt(2,2,1) - third2(2,1)
       ! dt(3,2,1) - third2(2,1)
       ! dt(4,2,1) - third2(2,1)
       ! dt(5,2,1) - third2(2,1)

       ! dt(1,1,2) - third2(1,2) -
       !           (first1=[first(2)(i1=3),first(2)(i1=-3)],
       !            second1=[second(2)(c1="+3"),second(2)(c1="-3") ] )
       ! dt(2,1,2) - third2(1,2)
       ! dt(3,1,2) - third2(1,2)
       ! dt(4,1,2) - third2(1,2)
       ! dt(5,1,2) - third2(1,2)

       ! dt(1,2,2) - third2(2,2) -
       !           (first1=[first(2)(i1=4),first(2)(i1=-4)],
       !            second1=[second(2)(c1="+4"),second(2)(c1="-4") ] )
       ! dt(2,2,2) - third2(2,2)
       ! dt(3,2,2) - third2(2,2)
       ! dt(4,2,2) - third2(2,2)
       ! dt(5,2,2) - third2(2,2)

        if(dt%k3 /= 1)                               error stop 46_4
        if(dt%l3 /= 2)                               error stop 47_4
        if(size(dt,1) /= 5)                          error stop 48_4
        if(size(dt,2) /= 2)                          error stop 49_4
        if(size(dt,3) /= 2)                          error stop 50_4

        do i=1,5
          if(dt(i,1,1)%first1%k1 /= 2)               error stop 51_4
          if(dt(i,1,1)%second1%l2 /= 2)              error stop 52_4

          if(lbound(dt(i,2,2)%first1,1) /= 0)        error stop 53_4
          if(ubound(dt(i,2,2)%first1,1) /= 1)        error stop 54_4
          if(lbound(dt(i,2,2)%second1,1) /= 2)       error stop 55_4
          if(ubound(dt(i,2,2)%second1,1) /= 3)       error stop 56_4

          if(any(dt(i,1,1)%first1%i1 /= [1,-1]))     error stop 57_4
          if(any(dt(i,2,1)%first1%i1 /= [2,-2]))     error stop 58_4
          if(any(dt(i,1,2)%first1%i1 /= [3,-3]))     error stop 59_4
          if(any(dt(i,2,2)%first1%i1 /= [4,-4]))     error stop 60_4

          if(any(dt(i,1,1)%second1%c1 /= ["+1","-1"]))       error stop 61_4
          if(any(dt(i,2,1)%second1%c1 /= ["+2","-2"]))       error stop 62_4
          if(any(dt(i,1,2)%second1%c1 /= ["+3","-3"]))       error stop 63_4
          if(any(dt(i,2,2)%second1%c1 /= ["+4","-4"]))       error stop 64_4
        end do
    end subroutine

    subroutine verify4(dt)
       type(third(1,*)),intent(in) :: dt(:,:,:)
       integer :: i

       ! element order:
       ! dt(1,1,1) -
       !            (first1=[first(2)(i1=1),first(2)(i1=-1)],
       !             second1=[second(2)(c1="+1"),second(2)(c1="-1") ] )
       ! dt(2,1,1) -
       !           (first1=[first(2)(i1=2),first(2)(i1=-2)],
       !            second1=[second(2)(c1="+2"), second(2)(c1="-2") ] )
       ! dt(1,2,1) -
       ! dt(2,2,1) -
       ! dt(1,3,1) -
       ! dt(2,3,1) -
       ! dt(1,4,1) -
       ! dt(2,4,1) -
       ! dt(1,5,1) -
       ! dt(2,5,1) -

       ! dt(1,1,2) -
       !           (first1=[first(2)(i1=3),first(2)(i1=-3)],
       !            second1=[second(2)(c1="+3"),second(2)(c1="-3") ] )
       ! dt(2,1,2) -
       !           (first1=[first(2)(i1=4),first(2)(i1=-4)],
       !            second1=[second(2)(c1="+4"),second(2)(c1="-4") ] )
       ! dt(1,2,2) -
       ! dt(2,2,2) -
       ! dt(1,3,2) -
       ! dt(2,3,2) -
       ! dt(1,4,2) -
       ! dt(2,4,2) -
       ! dt(1,5,2) -
       ! dt(2,5,2) -


        if(dt%k3 /= 1)                               error stop 65_4
        if(dt%l3 /= 2)                               error stop 66_4
        if(size(dt,1) /= 2)                          error stop 67_4
        if(size(dt,2) /= 5)                          error stop 68_4
        if(size(dt,3) /= 2)                          error stop 69_4

        do i=1,5
          if(dt(2,i,1)%first1%k1 /= 2)               error stop 70_4
          if(dt(2,i,1)%second1%l2 /= 2)              error stop 71_4

          if(lbound(dt(1,i,2)%first1,1) /= 0)        error stop 72_4
          if(ubound(dt(1,i,2)%first1,1) /= 1)        error stop 73_4
          if(lbound(dt(1,i,2)%second1,1) /= 2)       error stop 74_4
          if(ubound(dt(1,i,2)%second1,1) /= 3)       error stop 75_4

          if(any(dt(1,i,1)%first1%i1 /= [1,-1]))     error stop 76_4
          if(any(dt(2,i,1)%first1%i1 /= [2,-2]))     error stop 77_4
          if(any(dt(1,i,2)%first1%i1 /= [3,-3]))     error stop 78_4
          if(any(dt(2,i,2)%first1%i1 /= [4,-4]))     error stop 79_4

          if(any(dt(1,i,1)%second1%c1 /= ["+1","-1"]))       error stop 80_4
          if(any(dt(2,i,1)%second1%c1 /= ["+2","-2"]))       error stop 81_4
          if(any(dt(1,i,2)%second1%c1 /= ["+3","-3"]))       error stop 82_4
          if(any(dt(2,i,2)%second1%c1 /= ["+4","-4"]))       error stop 83_4
        end do

    end subroutine
end program

