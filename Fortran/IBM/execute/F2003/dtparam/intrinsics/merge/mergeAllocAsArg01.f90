!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeAllocAsArg01.f
!*
!*  DATE                       : Sept. 21 2008
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
!* 3. TSOURCE AND FSOURCE ARE ALLOCATABLE ARRAY
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k,l)
     integer,kind :: k
     integer,len  :: l

     integer(k)   :: i
     character(l) :: c
  end type
end module

program mergeAllocAsArg01
   use m
   implicit none

   integer :: k
   logical :: mask1(6)=[.true.,.false.,.true.,.false.,.true.,.false.]
   type(dtp(4,3)) :: dtp1(6),dtp2(2,3)
   type(dtp(4,3)),allocatable :: dtp3(:),dtp4(:,:)
   type(dtp(4,:)),allocatable :: dtp5(:),dtp6(:,:)
   logical :: mask2(2,3)

   mask2=reshape(mask1,(/2,3/))

   dtp1= [( (dtp(4,3)(i=k,c=char(k+64))),k=1,6 ) ]
   dtp2= reshape(dtp1,(/2,3/))

   allocate(dtp3(6),source= [( (dtp(4,3)(i=k+10,c=char(k+96))),k=1,6 ) ]  )
   allocate(dtp4(2,3),source=reshape(dtp3,(/2,3/)))

   dtp5=merge([dtp1(1),dtp1(2),dtp1(3)],[dtp3(4),dtp3(5),dtp3(6)], &
               [.true.,.false.,.true.])

   if(any(dtp5%i /= [1,15,3] ))                        error stop 11_4
   if(any(dtp5%c /= ["A","e","C"] ))                   error stop 12_4
   if(dtp5%k /= 4)                                     error stop 13_4
   if(dtp5%l /= 3)                                     error stop 14_4
   if(dtp5%i%kind /= 4)                                error stop 15_4
   if(dtp5%c%len /= 3)                                 error stop 16_4

   dtp5=merge([dtp3(1),dtp3(2),dtp3(3)],[dtp1(4),dtp1(5),dtp1(6)], &
               [.false.,.true.,.false.])

   if(any(dtp5%i /= [4,12,6] ))                        error stop 17_4
   if(any(dtp5%c /= ["D","b","F"] ))                   error stop 18_4
   if(dtp5%k /= 4)                                     error stop 19_4
   if(dtp5%l /= 3)                                     error stop 20_4
   if(dtp5%i%kind /= 4)                                error stop 21_4
   if(dtp5%c%len /= 3)                                 error stop 22_4

   dtp5=merge(dtp1(4:6),dtp3(1:3),[.true.,.false.,.true.])

   if(dtp5%k /= 4)                                     error stop 23_4
   if(dtp5%l /= 3)                                     error stop 24_4
   if(any(dtp5%i /= [4,12,6] ))                        error stop 25_4
   if(any(dtp5%c /= ["D","b","F"] ))                   error stop 26_4
   if(dtp5%k /= 4)                                     error stop 27_4
   if(dtp5%l /= 3)                                     error stop 28_4
   if(dtp5%i%kind /= 4)                                error stop 29_4
   if(dtp5%c%len /= 3)                                 error stop 30_4

   dtp5=merge(dtp1(1:5:2),dtp3(2:6:2),[.false.,.true.,.false.])

   if(dtp5%k /= 4)                                     error stop 31_4
   if(dtp5%l /= 3)                                     error stop 32_4
   if(any(dtp5%i /= [12,3,16] ))                       error stop 33_4
   if(any(dtp5%c /= ["b","C","f"] ))                   error stop 34_4
   if(dtp5%k /= 4)                                     error stop 35_4
   if(dtp5%l /= 3)                                     error stop 36_4
   if(dtp5%i%kind /= 4)                                error stop 37_4
   if(dtp5%c%len /= 3)                                 error stop 38_4

   dtp6=merge(dtp4,dtp2,mask2)

   if(dtp6%k /= 4)                                     error stop 39_4
   if(dtp6%l /= 3)                                     error stop 40_4
   if(any(dtp6(1,:)%i /= (/11,13,15/) ))               error stop 41_4
   if(any(dtp6(2,:)%i /= (/2,4,6/) ))                  error stop 42_4
   if(any(dtp6(1,:)%c /= ["a","c","e"]))               error stop 43_4
   if(any(dtp6(2,:)%c /= ["B","D","F"]))               error stop 44_4

   dtp6=merge(dtp4,dtp2,.not. mask2)

   if(dtp6%k /= 4)                                     error stop 45_4
   if(dtp6%l /= 3)                                     error stop 46_4
   if(any(dtp6(1,:)%i /= (/1,3,5/) ))                  error stop 47_4
   if(any(dtp6(2,:)%i /= (/12,14,16/) ))               error stop 48_4
   if(any(dtp6(1,:)%c /= ["A","C","E"]))               error stop 49_4
   if(any(dtp6(2,:)%c /= ["b","d","f"]))               error stop 50_4

end program
