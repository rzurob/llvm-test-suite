!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadSourceIsComp02.f   
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
!*  3. AND SOURCE IS POLYMORPHIC  
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
     integer,len       :: l1
     class(*),pointer  :: poly1(:)=>null()
  end type 
  type,extends(base) :: child(l2)
     integer,len     :: l2
     class(base(:)),pointer :: poly2(:)=>null()
  end type

end module

program spreadSourceIsComp02
  use m
  implicit none

  type(child(2,3)),target :: child1
  type(child(2,3)),target :: child2
  type(child(2,3)),target,allocatable :: tar(:)
  integer :: i,j  

  class(base(2)),pointer  :: base1=>null()

  allocate(child1%poly1(4:5),source=["123","456"])
  allocate(child2%poly1(-2:0),source=[1,2,3])

  tar=[child1,child2]
  allocate(base1,source=child(2,3)(child1%poly1,tar))

  select type(base1)
    type is(child(*,*))
      if(base1%l1 /= 2)                             error stop 10_4
      if(base1%l2 /= 3)                             error stop 11_4
      associate(x=>spread(base1%poly2,1,4))
        select type(x)
            type is(child(*,*))
            if(x%l1 /= 2)                           error stop 12_4
            if(x%l2 /= 3)                           error stop 13_4
            do i=1,4
               associate (y=>spread(x(i,1)%poly1,1,3))
                do j=1,3
                 select type(z=>y(j,1))
                   type is(character(*))
                     if(z /= "123")                 error stop 14_4
                   class default
                     error stop 102_4
                 end select

                 select type(z=>y(j,2))
                   type is(character(*))
                     if(z /= "456")                 error stop 15_4
                   class default
                     error stop 103_4
                 end select

                end do
               end associate 

               associate (y=>spread(x(i,2)%poly1,1,3))
                do j=1,3
                 select type(z=>y(j,1))
                   type is(integer(4))
                     if(z /= 1)                     error stop 16_4
                   class default
                     error stop 104_4
                 end select

                 select type(z=>y(j,2))
                   type is(integer(4))
                     if(z /= 2)                     error stop 17_4
                   class default
                     error stop 105_4
                 end select

                 select type(z=>y(j,3))
                   type is(integer(4))
                     if(z /= 3)                     error stop 18_4
                   class default
                      error stop 106_4
                 end select
                end do
               end associate
            end do
           type is(base(*))
              error stop 100_4 
          class default
              error stop 101_4 
        end select
      end associate

      associate(x=>spread(base1%poly1,1,4))
        do i=1,2
           select type(y=>x(i,1))
             type is(character(*))
               if(y /= "123")                     error stop 19_4
             class default
               error stop 107_4
           end select
           
           select type(y=>x(i,2))
             type is(character(*))          
                if(y /= "456")                    error stop 20_4
             class default
                error stop 108_4
           end select
        end do
      end associate
  end select
end program
