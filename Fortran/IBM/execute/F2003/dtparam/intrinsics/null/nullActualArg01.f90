!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullActualArg01.f
!*
!*  DATE                       : Sept. 24 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD])
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.88
!* 2. NULL([MOLD])
!* 3. MOLD IS POINTER OR ALLOCATABLE WITH DEFINED TYPE PARAMETER
!* 4. NULL([MOLD]) IS USED AS ACTUAL ARGUMENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l)
     integer,len :: l
     character(l),pointer     :: c1=>null()
     character(l),allocatable :: c2
   end type
end module

program nullActualArg01
   use m
   implicit none

   type(dtp(3)),pointer     :: dtp1=>null()
   type(dtp(3)),allocatable :: dtp2
   type(dtp(3)),pointer     :: dtp3(:)=>null()
   type(dtp(3)),allocatable :: dtp4(:)


   if(associated(dtp1))                            error stop 10_4
   if(associated(dtp3))                            error stop 11_4
   if(allocated(dtp2))                             error stop 12_4
   if(allocated(dtp4))                             error stop 13_4

   if(associated(dtp1))                            error stop 14_4
   if(associated(dtp3))                            error stop 15_4
   if(allocated(dtp2))                             error stop 16_4
   if(allocated(dtp4))                             error stop 17_4

   allocate(dtp1)
   allocate(dtp1%c1,source="123")
   allocate(dtp1%c2,source="456")

   allocate(dtp2)
   allocate(dtp2%c1,source="000")
   allocate(dtp2%c2,source="111")

   allocate(dtp3(2),source=dtp1)
   allocate(dtp4(2),source=dtp2)

   if(.not. associated(dtp1))                      error stop 18_4
   if(.not. associated(dtp3))                      error stop 19_4
   if(.not. allocated(dtp2))                       error stop 20_4
   if(.not. allocated(dtp4))                       error stop 21_4

   if(.not. associated(dtp1%c1))                   error stop 22_4
   if(.not. associated(dtp3(1)%c1))                error stop 23_4
   if(.not. associated(dtp3(2)%c1))                error stop 24_4

   if(.not. associated(dtp2%c1))                   error stop 25_4
   if(.not. associated(dtp4(1)%c1))                error stop 26_4
   if(.not. associated(dtp4(2)%c1))                error stop 27_4

   if(.not. allocated(dtp1%c2))                    error stop 28_4
   if(.not. allocated(dtp3(1)%c2))                 error stop 29_4
   if(.not. allocated(dtp3(2)%c2))                 error stop 30_4

   if(.not. allocated(dtp2%c2))                    error stop 31_4
   if(.not. allocated(dtp4(1)%c2))                 error stop 32_4
   if(.not. allocated(dtp4(2)%c2))                 error stop 33_4

   call nullChar1(null(dtp1%c1))
   call nullChar1(null())
   call nullChar1(null(dtp3(2)%c1))

   call nullChar1(null())
   call nullChar1(null(dtp4(1)%c1))
   call nullChar1(null())

   call nullChar2(null(dtp1%c2))
   call nullChar2(null(dtp3(1)%c2))
   call nullChar2(null())

   call nullChar2(null())
   call nullChar2(null())
   call nullChar2(null(dtp4(2)%c2))

   call nullDTP1(null(dtp1))
   call nullDTP3(null())

   call nullDTP2(null())
   call nullDTP4(null(dtp4))


   contains

      subroutine nullChar1(ch)
         character(3),pointer :: ch

         if(associated(ch))                        error stop 34_4
      end subroutine

      subroutine nullChar2(ch)
         character(3),allocatable :: ch

         if(allocated(ch))                         error stop 35_4
      end subroutine

      subroutine nullDTP1(dt)
         type(dtp(3)),pointer :: dt

         if(associated(dt))                        error stop 36_4
      end subroutine

      subroutine nullDTP2(dt)
         type(dtp(3)),allocatable :: dt

         if(allocated(dt))                         error stop 37_4
      end subroutine

      subroutine nullDTP3(dt)
         type(dtp(3)),pointer :: dt(:)

         if(associated(dt))                        error stop 38_4
      end subroutine

      subroutine nullDTP4(dt)
         type(dtp(3)),allocatable :: dt(:)

         if(allocated(dt))                         error stop 39_4
      end subroutine

end program
