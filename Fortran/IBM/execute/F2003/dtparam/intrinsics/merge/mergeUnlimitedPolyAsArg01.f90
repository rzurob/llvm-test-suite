!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeUnlimitedPolyAsArg01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 22 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75 
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK) 
!* 3. TSOURCE AND FSOURCE ARE UNLIMITED POLYMORPHIC ALLOCATABLE
!* 4. DEFECT 356275
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
     integer,kind :: k1
     integer,len  :: l1
    
     integer(k1)   :: i1
     character(l1) :: c1
  end type

  type,extends(base) :: child(k2,l2)
     integer(2),kind :: k2
     integer(2),len  :: l2
    
     integer(k2) :: i2
     character(l2) :: c2
  end type
end module

program mergeUnlimitedPolyAsArg01
   use m
   implicit none

   integer :: k
   logical :: mask1(6),mask2(2,3)

   class(*),allocatable :: poly1(:),poly2(:),poly5(:)
   class(*),allocatable :: poly3(:,:),poly4(:,:),poly6(:,:)

   type(child(4,3,2,6))  :: chld1(6),chld2(6)
   type(child(4,3,2,6))  :: chld3(2,3),chld4(2,3)

   mask1=[.true.,.false.,.true.,.false.,.true.,.false.]
   mask2=reshape(mask1,(/2,3/))
 
   chld1=[( (child(4,3,2,6)(i1=k,c1=char(k+64),i2=k+10,c2=char(k+96))),k=1,6 )]

   chld2=[( (child(4,3,2,6)(i1=-k,c1=char(k+70),i2=-k-10,c2=char(k+102))),k=1,6 )]

   chld3=reshape(source=chld1,shape=(/2,3/))

   chld4=reshape(source=chld2,shape=(/2,3/))

   allocate(poly1(6),source=chld1)

   allocate(poly2(6),source=chld2)

   allocate(poly3(2,3),source=chld3)
   
   allocate(poly4(2,3),source=chld4)

   allocate(poly5(1:1),source=merge(poly1(1:1),poly2(1:1),.true.))
   
   select type(poly5)
     type is(child(4,*,2,*))
       if(size(poly5,1) /= 1)                  error stop 10_4
       if(poly5%k1 /= 4)                       error stop 11_4
       if(poly5%l1 /= 3)                       error stop 12_4
       if(poly5%k1%kind /= 4)                  error stop 13_4
       if(poly5%l1%kind /= 4)                  error stop 14_4
       if(any(poly5%i1 /= 1))                  error stop 15_4
       if(any(poly5%c1 /= "A"))                error stop 16_4
       if(poly5%i1%kind /= 4)                  error stop 17_4
       if(poly5%c1%len /= 3)                   error stop 18_4

       if(poly5%k2 /= 2)                       error stop 19_4
       if(poly5%l2 /= 6)                       error stop 20_4
       if(poly5%k2%kind /= 2)                  error stop 21_4
       if(poly5%l2%kind /= 2)                  error stop 22_4
       if(any(poly5%i2 /= 11))                 error stop 23_4
       if(any(poly5%c2 /= "a"))                error stop 24_4
       if(poly5%i2%kind /= 2)                  error stop 25_4
       if(poly5%c2%len /= 6)                   error stop 26_4

       class default
           error stop 201_4
   end select

   if(allocated(poly5))  deallocate(poly5)
   
   allocate(poly5(1:1),source=merge(poly1(1:1),poly2(1:1),.false.))


   select type(poly5)
      type is(child(4,*,2,*))
         if(size(poly5,1) /= 1)                  error stop 30_4
         if(poly5%k1 /= 4)                       error stop 31_4
         if(poly5%l1 /= 3)                       error stop 32_4
         if(poly5%k1%kind /= 4)                  error stop 33_4
         if(poly5%l1%kind /= 4)                  error stop 34_4
         if(any(poly5%i1 /= -1))                 error stop 35_4
         if(any(poly5%c1 /= "G"))                error stop 36_4
         if(poly5%i1%kind /= 4)                  error stop 37_4
         if(poly5%c1%len /= 3)                   error stop 38_4

         if(poly5%k2 /= 2)                       error stop 39_4
         if(poly5%l2 /= 6)                       error stop 40_4
         if(poly5%k2%kind /= 2)                  error stop 41_4
         if(poly5%l2%kind /= 2)                  error stop 42_4
         if(any(poly5%i2 /= -11))                error stop 43_4
         if(any(poly5%c2 /= "g"))                error stop 44_4
         if(poly5%i2%kind /= 2)                  error stop 45_4
         if(poly5%c2%len /= 6)                   error stop 46_4
      class default
         error stop 202_4

   end select

   if(allocated(poly5))   deallocate(poly5)

   allocate(poly5(6),source=merge(poly1,poly2,mask1))

   select type(poly5)
      type is(child(4,*,2,*))
        if(size(poly5,1) /= 6)                  error stop 50_4
        if(poly5%k1 /= 4)                       error stop 51_4
        if(poly5%l1 /= 3)                       error stop 52_4
        if(poly5%k1%kind /= 4)                  error stop 53_4
        if(poly5%l1%kind /= 4)                  error stop 54_4
        if(any(poly5%i1 /= [1,-2,3,-4,5,-6]))      error stop 55_4
        if(any(poly5%c1 /= ["A","H","C","J","E","L"])) error stop 56_4
        if(poly5%i1%kind /= 4)                  error stop 57_4
        if(poly5%c1%len /= 3)                   error stop 58_4

        if(poly5%k2 /= 2)                       error stop 59_4
        if(poly5%l2 /= 6)                       error stop 60_4
        if(poly5%k2%kind /= 2)                  error stop 61_4
        if(poly5%l2%kind /= 2)                  error stop 62_4
        if(any(poly5%i2 /= [11,-12,13,-14,15,-16]))  error stop 63_4
        if(any(poly5%c2 /= ["a","h","c","j","e","l"]))  error stop 64_4
        if(poly5%i2%kind /= 2)                  error stop 65_4
        if(poly5%c2%len /= 6)                   error stop 66_4
     class default
        error stop 203_4

   end select

   if(allocated(poly5))  deallocate(poly5)

   allocate(poly5(6),source=merge(poly1,poly2,.not. mask1))

   
   select type(poly5)
     type is(child(4,*,2,*))
        if(size(poly5,1) /= 6)                  error stop 70_4
        if(poly5%k1 /= 4)                       error stop 71_4
        if(poly5%l1 /= 3)                       error stop 72_4
        if(poly5%k1%kind /= 4)                  error stop 73_4
        if(poly5%l1%kind /= 4)                  error stop 74_4
        if(any(poly5%i1 /= [-1,2,-3,4,-5,6]))   error stop 75_4
        if(any(poly5%c1 /= ["G","B","I","D","K","F"]))    error stop 76_4
        if(poly5%i1%kind /= 4)                  error stop 77_4
        if(poly5%c1%len /= 3)                   error stop 78_4

        if(poly5%k2 /= 2)                       error stop 79_4
        if(poly5%l2 /= 6)                       error stop 80_4
        if(poly5%k2%kind /= 2)                  error stop 81_4
        if(poly5%l2%kind /= 2)                  error stop 82_4
        if(any(poly5%i2 /= [-11,12,-13,14,-15,16]))       error stop 83_4
        if(any(poly5%c2 /= ["g","b","i","d","k","f"]))    error stop 84_4
        if(poly5%i2%kind /= 2)                  error stop 85_4
        if(poly5%c2%len /= 6)                   error stop 86_4
     class default
        error stop 204_4

   end select

   if(allocated(poly6))    deallocate(poly6)

   allocate(poly6(2,3),source=merge(poly3,poly4,mask2))
 
   select type(poly6)
      type is(child(4,*,2,*))
        if(size(poly6,1) /= 2)                  error stop 89_4
        if(size(poly6,2) /= 3)                  error stop 90_4
        if(poly6%k1 /= 4)                       error stop 91_4
        if(poly6%l1 /= 3)                       error stop 92_4
        if(poly6%k1%kind /= 4)                  error stop 93_4
        if(poly6%l1%kind /= 4)                  error stop 94_4
        if(any(poly6(1,:)%i1 /= [1,3,5]))       error stop 95_4
        if(any(poly6(2,:)%i1 /= [-2,-4,-6]))    error stop 96_4
        if(any(poly6(1,:)%c1 /= ["A","C","E"])) error stop 97_4
        if(any(poly6(2,:)%c1 /= ["H","J","L"])) error stop 98_4

        if(poly6%i1%kind /= 4)                  error stop 99_4
        if(poly6%c1%len /= 3)                   error stop 100_4
        if(poly6%k2 /= 2)                       error stop 101_4
        if(poly6%l2 /= 6)                       error stop 102_4
        if(poly6%k2%kind /= 2)                  error stop 103_4
        if(poly6%l2%kind /= 2)                  error stop 104_4
        if(any(poly6(1,:)%i2 /= [11,13,15]))    error stop 105_4
        if(any(poly6(2,:)%i2 /= [-12,-14,-16]))  error stop 106_4
        if(any(poly6(1,:)%c2 /= ["a","c","e"]))  error stop 107_4
        if(any(poly6(2,:)%c2 /= ["h","j","l"]))  error stop 108_4
        if(poly6%i2%kind /= 2)                  error stop 109_4
        if(poly6%c2%len /= 6)                   error stop 110_4
     class default
        error stop 205_4

  end select    

   if(allocated(poly6))  deallocate(poly6)

   allocate(poly6(2,3),source=merge(poly3,poly4,.not. mask2))
  
   select type(poly6)
      type is(child(4,*,2,*))
           if(size(poly6,1) /= 2)                  error stop 120_4
           if(size(poly6,2) /= 3)                  error stop 121_4
           if(poly6%k1 /= 4)                       error stop 122_4
           if(poly6%l1 /= 3)                       error stop 123_4
           if(poly6%k1%kind /= 4)                  error stop 124_4
           if(poly6%l1%kind /= 4)                  error stop 125_4
           if(any(poly6(1,:)%i1 /= [-1,-3,-5]))    error stop 126_4
           if(any(poly6(2,:)%i1 /= [2,4,6]))       error stop 127_4
           if(any(poly6(1,:)%c1 /= ["G","I","K"])) error stop 128_4
           if(any(poly6(2,:)%c1 /= ["B","D","F"])) error stop 129_4

           if(poly6%i1%kind /= 4)                  error stop 130_4
           if(poly6%c1%len /= 3)                   error stop 131_4
           if(poly6%k2 /= 2)                       error stop 132_4
           if(poly6%l2 /= 6)                       error stop 133_4
           if(poly6%k2%kind /= 2)                  error stop 134_4
           if(poly6%l2%kind /= 2)                  error stop 135_4
           if(any(poly6(1,:)%i2 /= [-11,-13,-15]))    error stop 136_4
           if(any(poly6(2,:)%i2 /= [12,14,16]))    error stop 137_4
           if(any(poly6(1,:)%c2 /= ["g","i","k"]))  error stop 138_4
           if(any(poly6(2,:)%c2 /= ["b","d","f"]))  error stop 139_4
           if(poly6%i2%kind /= 2)                  error stop 140_4
           if(poly6%c2%len /= 6)                   error stop 141_4
         class default
           error stop 206_4

    end select

end program
