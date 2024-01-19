!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 16 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK)
!* 3. DERIVED TYPE HAS EXTENDED TYPE
!* 4. COMPONENT IS UNLIMTED POLYMORPHIC
!* 5. TSOURCE AND FSOURCE ARE UNLIMITED POLYMORPHIC
!* 6. DEFECT 356275 355334
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(k,l)
      integer,kind :: k
      integer,len  :: l

      integer(k)   :: i
      character(l) :: c1
   end type
   type,extends(A) :: B(m)
      integer,len  :: m
      character(m) :: c2
      class(*),allocatable :: poly
   end type
end module

program mergeUnlimitedPolyComp01
   use m
   implicit none

   class(*),pointer     :: poly1=>null()
   class(*),allocatable :: poly2

   type(B(2,3,4)),target    :: b1=B(2,3,4)(i=10,c1="123",c2="456",poly=null())
   type(B(2,3,4))           :: b2=B(2,3,4)(i=20,c1="000",c2="111",poly=null())

   allocate(b1%poly,source=b1%A)
   allocate(b2%poly,source=b2%A)

   poly1=>b1
   allocate(poly2,source=b2)

   associate(x=>merge(poly1,poly2,.true.))
      select type(y=>x)
        type is(B(2,*,*))
          print *,y%c2,(y%c2 /= "456")
          if(y%k /= 2)                                   error stop 10_4
          if(y%l /= 3)                                   error stop 11_4
          if(y%m /= 4)                                   error stop 12_4
          if(y%i /= 10)                                  error stop 13_4
          if(y%c1 /= "123")                              error stop 14_4
          !--- defect 355334---!
          if(y%c2 /= "456")                              error stop 15_4
        class default
          error stop 100_4
      end select
   end associate

   associate(x=>merge(poly1,poly2,.false.))
      select type(y=>x)
        type is(B(2,*,*))
          print *,y%c2,(y%c2 /= "111")
          if(y%k /= 2)                                   error stop 16_4
          if(y%l /= 3)                                   error stop 17_4
          if(y%m /= 4)                                   error stop 18_4
          if(y%i /= 20)                                  error stop 19_4
          if(y%c1 /= "000")                              error stop 20_4
          !--- defect 355334---!
          if(y%c2 /= "111")                              error stop 21_4
        class default
          error stop 101_4
      end select
   end associate

   associate(x=>merge([poly1,poly1],[poly2,poly2],[.false.,.true.]))
      select type(y=>x)
        type is(B(2,*,*))
          if(y%k /= 2)                                   error stop 22_4
          if(y%l /= 3)                                   error stop 23_4
          if(y%m /= 4)                                   error stop 24_4
          if(any(y%i /= [20,10]))                        error stop 25_4
          if(any(y%c1 /=["000","123"]))                  error stop 26_4
          !--- defect 356275--!
          if(any(y%c2 /= ["111","456"]))                 error stop 27_4
        class default
          error stop 102_4
      end select
   end associate

   associate(x=>merge([b1%poly,b1%poly],[b2%poly,b2%poly],[.false.,.true.]))
      select type(y=>x)
        type is(A(2,*))
          if(y%k /= 2)                                   error stop 28_4
          if(y%l /= 3)                                   error stop 29_4
          if(any(y%i /= [20,10]))                        error stop 30_4
          if(any(y%c1 /=["000","123"]))                  error stop 31_4
        class default
          error stop 103_4
      end select
   end associate

   if(allocated(b1%poly))    deallocate(b1%poly)
   if(allocated(b2%poly))    deallocate(b2%poly)

   allocate(b1%poly,source=b1%i)
   allocate(b2%poly,source=b2%i)

   associate(x=>merge(b1%poly,b2%poly,.true.))
      select type(y=>x)
         type is(integer(2))
            if(y /= 10)                                  error stop 32_4
         class default
            error stop 104_4
      end select
   end associate

   associate(x=>merge([b1%poly,b1%poly],[b2%poly,b2%poly],[.false.,.true.]))
      select type(y=>x)
         type is(integer(2))
            if(any(y /= [20,10]))                        error stop 33_4
         class default
           error stop 105_4
      end select
   end associate

   if(allocated(b1%poly))    deallocate(b1%poly)
   if(allocated(b2%poly))    deallocate(b2%poly)

   allocate(b1%poly,source=b1%c1//b1%c2)
   allocate(b2%poly,source=b2%c1//b2%c2)

   associate(x=>merge(b1%poly,b2%poly,.true.))
      select type(y=>x)
         type is(character(*))
            if(y /= "123456")                            error stop 34_4
         class default
            error stop 106_4
      end select
   end associate

   associate(x=>merge([b1%poly,b1%poly],[b2%poly,b2%poly],[.false.,.true.]))
      select type(y=>x)
         type is(character(*))
            if(any(y /= ["000111","123456"]))            error stop 35_4
         class default
            error stop 107_4
      end select
   end associate

end program
