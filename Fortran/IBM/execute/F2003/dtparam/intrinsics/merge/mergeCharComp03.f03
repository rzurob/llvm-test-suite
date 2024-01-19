!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 11 2008
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
!* 3. TSOURCE,FSOURCE ARE DERIVED TYPE ARRAY OR SCALAR
!* 4. DERIVED TYPE HAS ALLOCATABLE CHARACTER ARRAY COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l)
     integer,len  :: l=3
     character(:),allocatable :: c1(:)
  end type
end module

program mergeCharComp03
   use m
   implicit none


   type(A(4)),target :: a1
   type(A(4)),target :: a2(2)
   type(A(:)),allocatable :: a3(:)
   type(A(:)),allocatable :: a4
   character(:),allocatable :: c1(:)
   type(A(:)),pointer:: a5
   type(A(:)),pointer :: a6(:)

   allocate(a1%c1(2),source=["12","34"])
   a1=merge(a1,A(4)(["56","78"]),.false.)

   if(a1%l /= 4)                                            error stop 10_4
   if(any(a1%c1 /= ["56","78"]) )                           error stop 11_4

   a2=(/A(4)(["abc","def"]),A(4)(["ghi","jkl"]) /)

   a3=merge(a2,a1,[.false.,.true.])
   if(a3%l /= 4)                                            error stop 12_4
   if(any(a3(1)%c1 /= ["56","78"]))                         error stop 13_4
   if(any(a3(2)%c1 /= ["ghi","jkl"]))                       error stop 14_4
   if(a3(1)%c1%len /= 2)                                    error stop 15_4
   if(a3(2)%c1%len /= 3)                                    error stop 16_4

   a4=merge(a2(1),a2(2),.false.)
   if(a4%l /= 4)                                            error stop 17_4
   if(any(a4%c1 /= ["ghi","jkl"]))                          error stop 18_4

   c1=merge(a1%c1,a2(2)%c1,[.true.,.false.])
   if(any(c1 /= ["56 ","jkl"]))                             error stop 19_4

   if(allocated(a3)) deallocate(a3)
   a3=merge(a2(1:2),A(4)(["12","34"]), [.true.,.false.])

   if(a3%l /= 4)                                           error stop 20_4
   if(any(a3(1)%c1 /= ["abc","def"]))                      error stop 21_4
   if(any(a3(2)%c1 /= ["12","34"]))                        error stop 22_4

   a5=>a1
   a6=>a2
   a6(1)=merge(a5,a1,.true.)
   a6(2)=merge(a5,a2(2),.false.)

   if(a6%l /= 4)                                           error stop 23_4
   if(any(a6(1)%c1 /= ["56","78"]))                        error stop 24_4
   if(any(a6(2)%c1 /= ["ghi","jkl"]))                      error stop 25_4

   a3=merge([A(1)(['a','b']),A(1)(['c','d'] )], &
            A(1)(['1','2']),[.true.,.false.] )
   if(a3%l /= 1)                                           error stop 26_4
   if(size(a3,1) /= 2)                                     error stop 27_4
   if(any(a3(1)%c1 /= ['a','b']))                          error stop 28_4
   if(any(a3(2)%c1 /= ['1','2']))                          error stop 29_4


end program
