!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocCharComp01.f
!*
!*  DATE                       : Oct. 3 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. COMPONENT IS CHARACTER ARRAY
!*  3. CALL MOVE_ALLOC IN TYPE_BOUND PROCEDURE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type mytype
      contains
         procedure,nopass :: proc1=>sub1
         procedure,nopass :: proc2=>sub2
         generic :: proc=>proc1,proc2
   end type
   type dtp(l)
      integer,len :: l
      character(:),allocatable :: c1(:)
      character(l),pointer     :: c2(:)=>null()
   end type
   contains
      subroutine sub1(from,to)
         type(dtp(*)),allocatable       :: from
         type(dtp(from%l)),allocatable  :: to

         call move_alloc(from,to)
      end subroutine

      subroutine sub2(from,to)
         type(dtp(*)),allocatable       :: from(:)
         type(dtp(from%l)),allocatable  :: to(:)

         call move_alloc(from,to)

      end subroutine
end module

program move_allocCharComp01

  use m
  implicit none

  integer   :: i

  type(mytype) :: mytype1

  type(dtp(5)),allocatable  :: from1
  type(dtp(5)),allocatable  :: to1
  type(dtp(7)),allocatable  :: from2(:)
  type(dtp(7)),allocatable  :: to2(:)

  allocate(dtp(5) :: from1)
  allocate(from1%c2(3),source=["IBM","XLF","XLC"])
  from1%c1=[character(7) :: "winter","summer","autumn","spring"]

  if(any(from1%c1 /= ["winter","summer","autumn","spring"] )) error stop 14_4

  allocate(from2(-4:-2))
  from2(-4)%c1 = ["a1","b2","c3"]
  from2(-3)%c1 = ["000","111","222"]
  from2(-2)%c1 = ["x","y","z"]

  do i=-4,-2
    allocate(from2(i)%c2(3),source=from2(i)%c1//":" )
  end do

  call mytype1%proc(from1,to1)
  if(allocated(from1))                             error stop 10_4
  if(.not. allocated(to1))                         error stop 11_4
  if(to1%l /= 5)                                   error stop 12_4
  if(to1%c1%len /= 7)                              error stop 13_4
  if(any(to1%c1 /= ["winter","summer","autumn","spring"])) error stop 14_4
  if(any(to1%c2 /= ["IBM","XLF","XLC"]))           error stop 15_4

  call mytype1%proc(from2,to2)
  if(allocated(from2))                             error stop 16_4
  if(.not. allocated(to2))                         error stop 17_4
  if(to2%l /= 7)                                   error stop 18_4
  if(to2(-4)%c1%len /= 2)                          error stop 19_4
  if(to2(-3)%c1%len /= 3)                          error stop 20_4
  if(to2(-2)%c1%len /= 1)                          error stop 21_4

  if(any(to2(-4)%c1 /= ["a1","b2","c3"]))          error stop 22_4
  if(any(to2(-3)%c1 /= ["000","111","222"]))       error stop 23_4
  if(any(to2(-2)%c1 /= ["x","y","z"]))             error stop 24_4

  if(to2(-4)%c2%len /= 7)                          error stop 25_4
  if(to2(-3)%c2%len /= 7)                          error stop 26_4
  if(to2(-2)%c2%len /= 7)                          error stop 27_4

  if(any(to2(-4)%c2 /= ["a1:","b2:","c3:"]))       error stop 28_4
  if(any(to2(-3)%c2 /= ["000:","111:","222:"]))    error stop 29_4
  if(any(to2(-2)%c2 /= ["x:","y:","z:"]))          error stop 30_4


end program

