!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeUnlimitedPolyComp02.f
!*
!*  DATE                       : Sept. 17 2008
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
!* 4. COMPONENT IS UNLIMTED POLYMORPHIC ARRAY
!* 5. TSOURCE AND FSOURCE ARE UNLIMITED POLYMORPHIC ARRAY
!* 6. DEFECT 356275
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
      class(*),allocatable :: poly(:)
   end type
end module

program mergeUnlimitedPolyComp02
   use m
   implicit none

   class(*),pointer     :: poly1(:)=>null()
   class(*),allocatable :: poly2(:)

   type(B(2,3,4)),target    :: b1(2) =  &
       [B(2,3,4)(i=10,c1="123",c2="456",poly=null()) , &
        B(2,3,4)(i=20,c1="AAA",c2="BBB",poly=null()) ]

   type(B(2,3,4))           :: b2(2) =  &
        [B(2,3,4)(i=30,c1="000",c2="111",poly=null()),  &
         B(2,3,4)(i=40,c1="CCC",c2="DDD",poly=null()) ]

   allocate(b1(1)%poly(2),source=b1(1)%A)
   allocate(b1(2)%poly(2),source=b1(2)%A)
   allocate(b2(1)%poly(2),source=b2(1)%A)
   allocate(b2(2)%poly(2),source=b2(2)%A)

   poly1(2:3)=>b1
   allocate(poly2(2),source=b2)

   associate(x=>merge(poly1,poly2,[.true.,.false.]))
      select type(y=>x)
        type is(B(2,*,*))
          if(y%k /= 2)                                   error stop 10_4
          if(y%l /= 3)                                   error stop 11_4
          if(y%m /= 4)                                   error stop 12_4
          if(any(y%i /= [10,40]))                        error stop 13_4
          if(any(y%c1 /= ["123","CCC"]))                 error stop 14_4
          !--- defect 356275--!
          if(any(y%c2 /= ["456","DDD"]))                 error stop 15_4
         class default
          error stop 100_4
      end select
   end associate

   associate(x=>merge(poly1,poly2,[.false.,.true.]))
      select type(y=>x)
        type is(B(2,*,*))
          if(y%k /= 2)                                   error stop 16_4
          if(y%l /= 3)                                   error stop 17_4
          if(y%m /= 4)                                   error stop 18_4
          if(any(y%i /= [30,20]))                        error stop 19_4
          if(any(y%c1 /= ["000","AAA"]))                 error stop 20_4
          !--- defect 356275---!
          if(any(y%c2 /= ["111","BBB"]))                 error stop 21_4
        class default
          error stop 101_4
      end select
   end associate

   associate(x=>merge(b1(1)%poly,b2(1)%poly,[.true.,.false.]))
      select type(x)
          type is(A(2,*))
             if(x%k /= 2)                                error stop 22_4
             if(x%l /= 3)                                error stop 23_4
             if(any(x%i /= [10,30]))                     error stop 24_4
             if(any(x%c1 /= ["123","000"]))              error stop 25_4
          class default
             error stop 102_4
      end select
   end associate

   associate(x=>merge(b1(2)%poly,b2(2)%poly,[.false.,.true.]))
      select type(x)
          type is(A(2,*))
             if(x%k /= 2)                                error stop 26_4
             if(x%l /= 3)                                error stop 27_4
             if(any(x%i /= [40,20]))                     error stop 28_4
             if(any(x%c1 /= ["CCC","AAA"]))              error stop 29_4
          class default
             error stop 103_4
      end select
   end associate

end program
