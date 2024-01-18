!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadSourceIsComp01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 26 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. SOURCE IS DERIVED TYPE COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m

  type inner1(k1,l1)
     integer,kind :: k1 
     integer,len  :: l1
     integer(k1)  :: i1(0:k1)
     character(l1) :: c1(-l1:1)
  end type 
  type inner2(k2,l2)
     integer,kind :: k2
     integer,len  :: l2 
     type(inner1(k2,l2)) :: inner1
  end type
  type inner3(k3,l3)
     integer,kind :: k3
     integer,len  :: l3
     type(inner2(2*k3,2*l3)) :: inner2
  end type 
  type outer(k4,l4)
     integer,kind :: k4
     integer,len  :: l4
     type(inner3(k4,l4)),allocatable :: inner3
  end type
end module

program spreadSourceIsComp01
  use m
  implicit none

  type(outer(1,1))             :: outer1
  type(outer(1,:)),allocatable :: outer2
    
  outer1%inner3=inner3(1,1)(inner2= &
                             inner2(2,2)(inner1= &
                             inner1(2,2)(i1=[1,2,3],c1=["a","b","c","d"])))

  allocate(outer(1,1) :: outer2)

  outer2%inner3=inner3(1,1)(inner2= &
                             inner2(2,2)(inner1= &
                             inner1(2,2)(i1=[4,5,6],c1=["g","h","i","j"]))) 

  call verify1(spread([outer1%inner3,outer2%inner3],1,4),1)
  call verify1(spread([outer1%inner3,outer2%inner3],1,4),2)

  call verify2(spread([outer1%inner3%inner2,outer2%inner3%inner2],1,4),1)
  call verify2(spread([outer1%inner3%inner2,outer2%inner3%inner2],1,4),2)

  call verify3(spread([outer1%inner3%inner2%inner1, &
                       outer2%inner3%inner2%inner1],1,4),1)

  call verify3(spread([outer1%inner3%inner2%inner1, &
                       outer2%inner3%inner2%inner1],1,4),2)


  call verify4(spread(outer1%inner3%inner2%inner1%i1,1,4),1)
  call verify4(spread(outer1%inner3%inner2%inner1%i1,1,4),2)

  call verify5(spread(outer1%inner3%inner2%inner1%c1,1,4),1)
  call verify5(spread(outer1%inner3%inner2%inner1%c1,1,4),2)

  contains

  subroutine verify1(dt,dim)
     type(inner3(1,*)),intent(in) :: dt(:,:)
     integer :: i,dim
     if(dt%k3 /= 1)                                   error stop 10_4
     if(dt%l3 /= 1)                                   error stop 11_4
     do i=1,4
       select case(dim)
       case (1)
         associate(x=>dt(i,1)%inner2)
           if(x%k2 /= 2)                              error stop 12_4
           if(x%l2 /= 2)                              error stop 13_4
           associate(y1=>x%inner1)
             if(y1%k1 /= 2)                           error stop 14_4
             if(y1%l1 /= 2)                           error stop 15_4
             if(any(y1%i1 /= [1,2,3]))                error stop 16_4
             if(any(y1%c1 /= ["a","b","c","d"]))      error stop 17_4
           end associate
         end associate

         associate(x=>dt(i,2)%inner2)
           if(x%k2 /= 2)                              error stop 18_4
           if(x%l2 /= 2)                              error stop 19_4
           associate(y1=>x%inner1)
             if(y1%k1 /= 2)                           error stop 20_4
             if(y1%l1 /= 2)                           error stop 21_4
             if(any(y1%i1 /= [4,5,6]))                error stop 22_4
             if(any(y1%c1 /= ["g","h","i","j"]))      error stop 23_4
           end associate
         end associate
       case (2)
         associate(x=>dt(1,i)%inner2)
           if(x%k2 /= 2)                              error stop 24_4
           if(x%l2 /= 2)                              error stop 25_4
           associate(y1=>x%inner1)
             if(y1%k1 /= 2)                           error stop 26_4
             if(y1%l1 /= 2)                           error stop 27_4
             if(any(y1%i1 /= [1,2,3]))                error stop 28_4
             if(any(y1%c1 /= ["a","b","c","d"]))      error stop 29_4
           end associate
         end associate

         associate(x=>dt(2,i)%inner2)
           if(x%k2 /= 2)                              error stop 30_4
           if(x%l2 /= 2)                              error stop 31_4
           associate(y1=>x%inner1)
             if(y1%k1 /= 2)                           error stop 32_4
             if(y1%l1 /= 2)                           error stop 33_4
             if(any(y1%i1 /= [4,5,6]))                error stop 34_4
             if(any(y1%c1 /= ["g","h","i","j"]))      error stop 35_4
           end associate
         end associate
       end select
     end do  
  end subroutine 


  subroutine verify2(dt,dim)
     type(inner2(2,*)),intent(in) :: dt(:,:)
     integer :: i,dim
     if(dt%k2 /= 1)                                   error stop 36_4
     if(dt%l2 /= 1)                                   error stop 37_4
     do i=1,4
       select case(dim)
       case (1)
         associate(x=>dt(i,1)%inner1)
           if(x%k1 /= 2)                              error stop 38_4
           if(x%l1 /= 2)                              error stop 39_4
             if(any(x%i1 /= [1,2,3]))                 error stop 40_4
             if(any(x%c1 /= ["a","b","c","d"]))       error stop 41_4
         end associate

         associate(x=>dt(i,2)%inner1)
           if(x%k1 /= 2)                              error stop 42_4
           if(x%l1 /= 2)                              error stop 43_4
             if(any(x%i1 /= [4,5,6]))                 error stop 44_4
             if(any(x%c1 /= ["g","h","i","j"]))       error stop 45_4
         end associate
       case (2)
         associate(x=>dt(1,i)%inner1)
           if(x%k1 /= 2)                              error stop 46_4
           if(x%l1 /= 2)                              error stop 47_4
             if(any(x%i1 /= [1,2,3]))                 error stop 48_4
             if(any(x%c1 /= ["a","b","c","d"]))       error stop 49_4
         end associate

         associate(x=>dt(2,i)%inner1)
           if(x%k1 /= 2)                              error stop 50_4
           if(x%l1 /= 2)                              error stop 51_4
             if(any(x%i1 /= [4,5,6]))                 error stop 52_4
             if(any(x%c1 /= ["g","h","i","j"]))       error stop 53_4
         end associate

       end select
     end do
  end subroutine

  subroutine verify3(dt,dim)
     type(inner1(2,*)),intent(in) :: dt(:,:)
     integer :: i,dim
     if(dt%k1 /= 2)                                   error stop 54_4
     if(dt%l1 /= 2)                                   error stop 55_4
     do i=1,4
       select case(dim)
       case (1)
         associate(x=>dt(i,1))
             if(any(x%i1 /= [1,2,3]))                 error stop 56_4
             if(any(x%c1 /= ["a","b","c","d"]))       error stop 57_4
         end associate

         associate(x=>dt(i,2))
             if(any(x%i1 /= [4,5,6]))                 error stop 58_4
             if(any(x%c1 /= ["g","h","i","j"]))       error stop 59_4
         end associate
       case (2)
         associate(x=>dt(1,i))
             if(any(x%i1 /= [1,2,3]))                 error stop 60_4
             if(any(x%c1 /= ["a","b","c","d"]))       error stop 61_4
         end associate

         associate(x=>dt(2,i))
             if(any(x%i1 /= [4,5,6]))                 error stop 62_4
             if(any(x%c1 /= ["g","h","i","j"]))       error stop 63_4
         end associate

       end select
     end do
  end subroutine

  subroutine verify4(int,dim)
     integer(2),intent(in) :: int(:,:)
     integer :: i,dim
     do i=1,4
       select case(dim)
       case (1)
         associate(x=>int(i,1))
             if(any(x /= [1,2,3]))                    error stop 64_4
         end associate

         associate(x=>int(i,2))
             if(any(x /= [4,5,6]))                    error stop 65_4
         end associate
       case (2)
         associate(x=>int(1,i))
             if(any(x /= [1,2,3]))                    error stop 66_4
         end associate

         associate(x=>int(2,i))
             if(any(x /= [4,5,6]))                    error stop 67_4
         end associate

       end select
     end do
  end subroutine

  subroutine verify5(ch,dim)
     character(*),intent(in) :: ch(:,:)
     integer :: i,dim
     do i=1,4
       select case(dim)
       case (1)
         associate(x=>ch(i,1))
             if(any(x /= ["a","b","c","d"]))          error stop 68_4
         end associate

         associate(x=>ch(i,2))
             if(any(x /= ["g","h","i","j"]))          error stop 69_4
         end associate
       case (2)
         associate(x=>ch(1,i))
             if(any(x /= ["a","b","c","d"]))          error stop 70_4
         end associate

         associate(x=>ch(2,i))
             if(any(x /= ["g","h","i","j"]))          error stop 71_4
         end associate

       end select
     end do
  end subroutine
end program
